# create by Thomas Dufour & Pierre Moreau, oct. 2018
library(shiny)
library(shinydashboard)
library(xlsx)
library(fastDummies)
library(readxl)
library(stats)
library(stringr)
library(ggplot2)
library(dplyr)
library(plotly)
library(stringr)
library(sqldf)        # Using SQLite in R to manipulate dataframes :D

survey = read.xlsx("surveydataece.xlsx", sheetIndex=1)
logs = read.csv2("logs.csv")
logs$Time<-format(strptime(as.character(logs$Time), "%d/%m/%Y"), "%Y-%m-%d") #dealing with time format

# Remove accents
Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}


# Process user names 
processLogsUsernames <- function(logsUsers){
  logsUsers = Unaccent(logsUsers)
  logsUsers = str_replace_all(logsUsers, 'Z', 'e')
  logsUsers = str_replace(logsUsers, "ftienne", "Etienne")
  logsUsers = str_replace(logsUsers, "flie", "Elie")
  logsUsers = str_replace(logsUsers, "In?s", "Ines")
  logsUsers = str_replace(logsUsers, "fdouard", "Edouard")
  
  return(logsUsers)
}

processLogs <- function(logs){
  # Process user names
  logs$User <-processLogsUsernames(logs$User)
  
  # Remove the null columns  (keep only the first 5 columns)
  logs <- logs[c(1:5)]
  
  # Compute Day number = datedif("day", Time, min(Time) of user)
  # Since I love SQL and I already did the computation in SQL in Tableau Software, I'm using the SQL package
  logs = sqldf("select l.*, d.FirstDate
              from logs l
              left join (
              -- I compute the minimim date for each user
              -- and add 1 day such that the first behaviour day will be the -1 day
                  select User, date(min(Time), '+1 day') as FirstDate from logs group by User
              )d on l.User = d.User")

  # We compute the day difference between the log input and the first log input
  logs$Day <- as.numeric(as.Date.factor(logs$Time) - as.Date.factor(logs$FirstDate))

  # We remove the logs where day = -1 (the behaviour week starts always 1 day too early)
  logs = logs[logs$Day >= 0,]

  # Compute week number = floor(Day#/7)
  logs$Week <- floor(logs$Day/7)
  
  # Compute the habit KPI
  logs = sqldf("select logs.*, d.Habit_d, d.Habit_w
              from logs
              left join (
                SELECT User,
                COUNT(*)/7.0 as Habit_d,
                COUNT(*) as Habit_w
                FROM logs WHERE Type = 'Behaviour' GROUP BY User
              )d on logs.User = d.User")
  
  # Those KPIs are computed on a Per Week basis : 
  logs.kpis_w = sqldf("SELECT User, Week,
                      SUM(case when Type='On time' then 1 else 0 end) Ontime_w,
                      SUM(case when Type='Cheated' then 1 else 0 end) Cheated_w,
                      SUM(case when Type='Skipped' then 1 else 0 end) Skipped_w,
                      SUM(case when Type='Auto skipped' then 1 else 0 end) Autoskipped_w,
                      SUM(case when Type='Friend' then 1 else 0 end) Friend_w
                    FROM logs GROUP BY User, Week ")
  # Left join those weekly KPIs with logs
  logs = merge(x = logs, y = logs.kpis_w, by = c("User", "Week"), all.x = TRUE)
  
  logs$Plan_w     <- logs$Ontime_w + logs$Skipped_w + logs$Autoskipped_w
  logs$Smoked_w   <- logs$Ontime_w + logs$Cheated_w
  logs$Progress_w <- logs$Habit_w - logs$Smoked_w
  logs$Effort_w   <- logs$Plan_w - logs$Smoked_w
  
  # Be careful, there are NaN values in Activity (only for Behaviour and Friend types rows) and Inf values
  logs$Activity_w <- logs$Smoked_w*1.0/logs$Plan_w
  
  # Compute daily kpis in order to compute the in-week correlation between 2 variables
  logs.kpis_d = sqldf("SELECT User, Day, Week,
             SUM(case when Type='Cheated' then 1 else 0 end) Cheated_d,
                      SUM(case when Type='Skipped' then 1 else 0 end) Skipped_d
                      FROM logs GROUP BY User, Week, Day ")
  # Compute the Pearson's correlation along the week and agregate in Week
  logs.kpis_w = logs.kpis_d %>% group_by(User, Week) %>% summarise(C=cor(Cheated_d,Skipped_d))
  # Merge those KPIs with the logs
  logs = merge(x = logs, y = logs.kpis_w, by = c("User", "Week"), all.x = TRUE)
  # Fill 0 valued correlation to NA :
  logs$C[is.na(logs$C)] <- 0
  
  # Engagement = C * V :
  logs$Engagement_w <- logs$C * (logs$Skipped_w + logs$Cheated_w)/(logs$Plan_w + 1.0)
  
  # Add conditional Active and Engaged KPIs
  logs = sqldf("SELECT *,
              CASE WHEN Activity_w > 0.3 THEN 1 ELSE 0 END AS Active_w,
              CASE WHEN Activity_w > 0.3 AND Engagement_w < 0.6 THEN 1 ELSE 0 END AS Engaged_w
             FROM logs")

  return(logs)
}

#dealing with the encoding format and special characters
processSurveyUsernames <- function(surveyUsers){
  surveyUsers = str_replace_all(surveyUsers, "é", "e")
  surveyUsers = str_replace_all(surveyUsers, "É", "E")
  surveyUsers = str_replace_all(surveyUsers, "ë", "e")
  surveyUsers = str_replace_all(surveyUsers, "è", "e")
  surveyUsers = str_replace_all(surveyUsers, "Ã©", "e")
  surveyUsers = str_replace_all(surveyUsers, "Ã¨", "e")
  surveyUsers = str_replace_all(surveyUsers, "Ã‰", "E")
  surveyUsers = str_replace_all(surveyUsers, "Ã«", "e")
  return(surveyUsers)
}

processSurvey <- function(survey){
  survey$Name <-processSurveyUsernames(unique(survey$Name))
  
  # We add the age category indicator : Youngs = 1, mids = 2, olds = 3
  survey = survey %>%
    mutate(Age.category = case_when(Age < 30 ~ 1,
                               Age >= 30 & Age < 50 ~ 2,
                               Age >= 50 ~ 3))
  
  return(survey)
}


survey<-processSurvey(survey)
logs<-processLogs(logs)

# Keep 1 line per user and per week, with all the corresponding KPIs
logs_weekly = logs[c(1,2,11:ncol(logs))] %>% group_by(User=logs$User, Week=logs$Week) %>% summarise_each(funs(mean))

user<-"Audrey Auberjonois" #initializing the user variable for the beginning



ui<- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Single User", tabName = "Single_user",
               icon = icon("dashboard")),
      menuItem("All Users", tabName = "all_users",
               icon = icon("users", class = "fas", lib = "font-awesome")),
      menuItem("Github", icon = icon("github", class = "fab", lib = "font-awesome"),
               href= "https://github.com/Mifour/DataAnalyisR")
    )
  ),
  body <- dashboardBody(
    tabItems(
      tabItem("Single_user", 
              fluidRow(
                column(3, align="center",
                       selectInput("user", "User", choices = unique(logs$User), selected = user ),
                       verbatimTextOutput("userGender"),
                       verbatimTextOutput("userAge"),
                       verbatimTextOutput("BMI"), br() ,
                       
                       h3("Savings"), 
                       "Cigs saved: ", textOutput("saved"), br(),
                       "Money saved (L£): ", textOutput("moneySaved"),
                       br(),
                       
                       h3("Current Activities"), 
                       textOutput("currentActivity"),
                       textOutput("currentEngaged"),
                       br(),
                       plotlyOutput("plot0")
                       
                       
                ),
                column(9,
                       
                       plotlyOutput("plot1"),
                       
                       plotlyOutput("plot2"),
                       
                       plotlyOutput("plot3")
                )
                
              ),    
              tags$footer(tags$em("Created by Thomas Dufour & Pierre Moreau"))
      ),
      tabItem("all_users", "Widgets tab content")
    )
   
  )
)

server = function(input, output) {
  #manipulating the data
  
  # Sort Logs on Users 
  logs<-logs[order(logs$User),]
  
  user_vals <- reactiveValues()
  observe({
    user_vals$user <- input$user
    # Compute Absolute KPIs for user
    user_vals$habits_d <- mean(logs$Habit_d[(logs$User == input$user)])
    user_vals$smoked <- sum(logs_weekly$Smoked_w[(logs_weekly$User == user)])
    user_vals$saved <- floor(user_vals$habits_d*(max(logs$Day[(logs$User == user)])-6) - user_vals$smoked)
    user_vals$last_log_weekly <- logs_weekly %>% arrange(desc(logs_weekly$Week)) %>% filter(User == input$user) %>% slice(1)
    user_vals$current_activity <- user_vals$last_log_weekly$Active_w
    user_vals$current_engaged <- user_vals$last_log_weekly$Engaged_w
  })
  
  #getting the user info as reactive variable between server and client
  output$userGender<-reactive(as.character(survey$Gender[survey$Name == input$user]))
  output$userAge<-reactive(as.character(paste(survey$Age[survey$Name == input$user],"years old")))
  output$BMI<-reactive(as.character(paste("BMI :",round(survey$weigh[survey$Name == input$user]/((survey$height[survey$Name == input$user]/100)**2))) ))
  
  #calcutlating the difference between the cigs consuption before and now (by integral diference)
  output$saved <- renderText({user_vals$saved})
  
  #and the amount of money saved, in libanese pounds
  output$moneySaved <-renderText({user_vals$saved*3475/20}) 
  
  output$currentActivity <- renderText(paste("Active :",{user_vals$current_activity}))
  output$currentEngaged <- renderText(paste("Engaged :",{user_vals$current_engaged}))
  
  #counting the logs type to know how much the different features are used
  output$plot0 <- renderPlotly({ 
    values <- data.frame(value = logs$Type[logs$User== input$user])
    nr.of.appearances <- aggregate(x = values, 
                                   by = list(unique.values = values$value), 
                                   FUN = length)
    nr.of.appearances$value <- round(100*nr.of.appearances$value/(sum(nr.of.appearances$value)))
    plot_ly(x=nr.of.appearances$value,
            y=nr.of.appearances$unique.values,
            name="Features Ratio",
            type="bar" ,
            orientation = 'h',
            color = nr.of.appearances$unique.values, colors = "Set1",
            text = nr.of.appearances$value, textposition = 'auto')%>%
    config(displayModeBar = F)%>%
    layout( title = "Features ratio (%)", showlegend = FALSE)
  }) 
  
  #rendering the first graph, about the user's engagement over time, per week
  output$plot1 <- renderPlotly({ 
      df = data.frame(engagement = logs_weekly$Engagement_w[logs_weekly$User == input$user & logs_weekly$Week>0], 
                      engaged    = logs_weekly$Engaged_w[logs_weekly$User == input$user & logs_weekly$Week>0], 
                      active     = 1-logs_weekly$Active_w[logs_weekly$User == input$user & logs_weekly$Week>0], 
                      week       = logs_weekly$Week[logs_weekly$User == input$user & logs_weekly$Week>0]) 
      
      p1 <- plot_ly(x=df$week, y=df$engaged, name="Engaged", type = 'scatter', mode = 'lines',
                    line = list(color = '#00b159', width = 2,shape = "hvh"),
                    fill = 'tozeroy', fillcolor = '#00b159')%>%
        # add_lines(x=df$week, y = df$engaged, name= "Engaged",type = 'scatter', mode = 'lines', color='green') %>%
        add_lines(x=df$week, y = df$active, name= "non-activity",type = 'scatter', mode = 'lines',
                  line = list(color = '#d11141', width = 2,shape = "hvh"),
                  fill = 'tozeroy', fillcolor = '#d11141') %>%
      config(displayModeBar = F)%>%
      layout( title = "How much engaged was the user", hovermode = 'compare')
     
  }) 
  
  #rendering the first graph, about the user's effort over time, per week
  output$plot2 <- renderPlotly({ 
    df = data.frame(efforts = logs_weekly$Effort_w[logs_weekly$User == input$user & logs_weekly$Week>0], 
                    progress = logs_weekly$Progress_w[logs_weekly$User == input$user & logs_weekly$Week>0],
                    week = logs_weekly$Week[logs_weekly$User == input$user & logs_weekly$Week>0]) 
    
    p1 <- plot_ly(x=df$week, y=df$efforts, name="Effort", type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
      add_lines(x=df$week, y = df$progress, name= "progress ",type = 'scatter', mode = 'lines')%>%
    config(displayModeBar = F)%>%
    layout( title = "How many efforts did the user", hovermode = 'compare')
  }) 
  
  #rendering the first graph, about the user's consuption over time, per week
  output$plot3 <- renderPlotly({ 
    df = data.frame(values = logs_weekly$Plan_w[logs_weekly$User == input$user & logs_weekly$Week>0], 
                    week = logs_weekly$Week[logs_weekly$User == input$user & logs_weekly$Week>0]) 
    habit = user_vals$habits_d*7
    
    p1 <- plot_ly(x=df$week, y=df$values, name="Plan", type="bar")%>%
      add_lines(x=df$week, y = habit, name= "Habit ",type = 'scatter', mode = 'lines',
                    line = list( width = 3, dash='dash')
                ) %>%
    config(displayModeBar = F)%>%
    layout( title = "How did the consuption evolved", hovermode = 'compare')
    }) 

  
}
shinyApp(ui, server)