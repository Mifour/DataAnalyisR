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

# Magical operator to return multiple values from functions
# You can use it like this : 
#   t <- function(){ return(list(1, 2)) }
#   c(a, b) := t()

# ':=' <- function(lhs, rhs) {
#   frame <- parent.frame()
#   lhs <- as.list(substitute(lhs))
#   if (length(lhs) > 1)
#     lhs <- lhs[-1]
#   if (length(lhs) == 1) {
#     do.call(`=`, list(lhs[[1]], rhs), envir=frame)
#     return(invisible(NULL))
#   }
#   if (is.function(rhs) || is(rhs, 'formula'))
#     rhs <- list(rhs)
#   if (length(lhs) > length(rhs))
#     rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
#   for (i in 1:length(lhs))
#     do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
#   return(invisible(NULL))
# }


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
  
  # Compute the weekday
  logs$Weekday <- weekdays(as.Date(logs$Time))
  
  # Compute the habit KPI
  logs = sqldf("select logs.*, d.Habit_d, d.Habit_w
               from logs
               left join (
               SELECT User,
               COUNT(*)/7.0 as Habit_d,
               COUNT(*) as Habit_w
               FROM logs WHERE Type = 'Behaviour' GROUP BY User
               )d on logs.User = d.User")
   
  # Those KPIs are computed on a Per Day basis : 
  logs.kpis_d = sqldf("SELECT User, Week, Day,
                      SUM(case when Type='On time' then 1 else 0 end) Ontime_d,
                      SUM(case when Type='Cheated' then 1 else 0 end) Cheated_d,
                      SUM(case when Type='Skipped' then 1 else 0 end) Skipped_d,
                      SUM(case when Type='Snoozed' then 1 else 0 end) Snoozed_d,
                      SUM(case when Type='Auto skipped' then 1 else 0 end) Autoskipped_d,
                      SUM(case when Type='Friend' then 1 else 0 end) Friend_d,
                      SUM(case when Type='Behaviour' then 1 else 0 end) Behaviour_d
                      FROM logs GROUP BY User, Week, Day ") 
  # Left join those weekly KPIs with logs
  logs = merge(x = logs, y = logs.kpis_d, by = c("User", "Week","Day"), all.x = TRUE)
  
  # Those KPIs are computed on a Per Week basis : 
  logs.kpis_w = sqldf("SELECT User, Week,
                      SUM(case when Type='On time' then 1 else 0 end) Ontime_w,
                      SUM(case when Type='Cheated' then 1 else 0 end) Cheated_w,
                      SUM(case when Type='Skipped' then 1 else 0 end) Skipped_w,
                      SUM(case when Type='Snoozed' then 1 else 0 end) Snoozed_w,
                      SUM(case when Type='Auto skipped' then 1 else 0 end) Autoskipped_w,
                      SUM(case when Type='Friend' then 1 else 0 end) Friend_w,
                      SUM(case when Type='Behaviour' then 1 else 0 end) Behaviour_w
                      FROM logs GROUP BY User, Week ")
  # Left join those weekly KPIs with logs
  logs = merge(x = logs, y = logs.kpis_w, by = c("User", "Week"), all.x = TRUE)
  
  logs$Plan_d        <- logs$Ontime_d + logs$Skipped_d + logs$Autoskipped_d
  logs$Smoked_d      <- logs$Ontime_d + logs$Cheated_d
  logs$Consumption_d <- logs$Smoked_d + logs$Behaviour_d
  logs$Progress_d    <- logs$Habit_d - logs$Smoked_d
  logs$Effort_d      <- logs$Plan_d - logs$Smoked_d
  logs$Engagement2_d <- 1 - logs$Autoskipped_d/(logs$Autoskipped_d+logs$Skipped_d+logs$Ontime_d+logs$Snoozed_d)
  
  logs$Plan_w        <- logs$Ontime_w + logs$Skipped_w + logs$Autoskipped_w
  logs$Smoked_w      <- logs$Ontime_w + logs$Cheated_w
  logs$Consumption_w <- logs$Smoked_w + logs$Behaviour_w
  logs$Progress_w    <- logs$Habit_w - logs$Smoked_w
  logs$Effort_w      <- logs$Plan_w - logs$Smoked_w
  logs$Engagement2_w <- 1 - logs$Autoskipped_w/(logs$Autoskipped_w+logs$Skipped_w+logs$Ontime_w+logs$Snoozed_w)
  
  # Be careful, there are NaN values in Activity (only for Behaviour and Friend types rows) and Inf values
  logs$Activity_w <- logs$Smoked_w*1.0/logs$Plan_w
  
  # # Compute daily kpis in order to compute the in-week correlation between 2 variables
  # logs.kpis_d = sqldf("SELECT User, Day, Week,
  #            SUM(case when Type='Cheated' then 1 else 0 end) Cheated_d,
  #                     SUM(case when Type='Skipped' then 1 else 0 end) Skipped_d
  #                     FROM logs GROUP BY User, Week, Day ")
  
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
               CASE WHEN Activity_w > 0.3 AND Engagement_w < 0.6 THEN 1 ELSE 0 END AS Engaged_w,
               CASE WHEN Engagement2_d > 0.4 THEN 1 ELSE 0 END AS Engaged2_d,
               CASE WHEN Engagement2_w > 0.4 THEN 1 ELSE 0 END AS Engaged2_w
               FROM logs")
  
  # Compute corrected consumption
  # <- logs_weekly %>% arrange(asc(logs_weekly$Week)) %>% filter(User == input$user)
  
  logs_weekly = select(logs, User, Week, ends_with("_w")) %>% group_by(User=logs$User, Week=logs$Week) %>% summarise_each(funs(mean))
  logs_weekly = logs_weekly %>% arrange(logs_weekly$Week)
  logs_weekly$Consumption_w_corrected = logs_weekly$Consumption_w
  
  for(i in 1:nrow(logs_weekly)) {
    # edit the corrected consumption on logs_weekly[i,]
    if(logs_weekly[i,]$Engaged2_w==0 & logs_weekly[i,]$Week>0) {
      C = select(logs_weekly,User, Week, Consumption_w_corrected) %>% filter(User==logs_weekly[i,]$User & Week==logs_weekly[i,]$Week-1)
      if(nrow(C)>0){logs_weekly[i,]$Consumption_w_corrected = C[[1,3]]}
    }
  }
  # Merging those 2 tables (adding the corrected consumption to logs)
  logs = sqldf("SELECT l.*, lw.Consumption_w_corrected
               FROM logs l LEFT JOIN logs_weekly lw ON l.User = lw.User AND l.Week = lw.Week")
  
  
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

# computeStats <- function(logs, logs_weekly){
#   
#   user_max_day <- logs %>% group_by(User=logs$User) %>% summarize(max_day = max(Day))
#   
#   user_stats= logs_weekly %>% group_by(User) %>% summarise(
#     max_week = max(Week),
#     habits_d = mean(Behaviour_w, na.rm = T)/7.,
#     habits_w = mean(Behaviour_w, na.rm = T),
#     total_smoked = sum(Smoked_w),
#     avg_smoked = mean(Smoked_w, na.rm = T),
#     total_consumption = sum(Consumption_w),
#     avg_consumption = mean(Consumption_w, na.rm = T),
#     avg_engagement2_w = mean(Engagement2_w, na.rm = T)
#   )
#   
#   user_stats = merge(x = user_stats, y = user_max_day, by = "User", all.x = TRUE)
#   user_stats$total_saved = floor(user_stats$habits_d*(user_stats$max_day-6) - user_stats$total_smoked)
#   user_stats$money_saved = user_stats$total_saved*3475/20
#   
#   all_user_stats = user_stats %>% summarize(
#     total_smoked = sum(total_smoked),
#     avg_smoked = mean(avg_smoked, na.rm = T),
#     total_consumption = sum(total_consumption),
#     avg_consumption = mean(avg_consumption, na.rm = T),
#     avg_engagement2_w = mean(avg_engagement2_w, na.rm = T)
#   )
#   
#   return(list(user_stats, all_user_stats))
# }


survey<-processSurvey(survey)
logs<-processLogs(logs)

# Keep 1 line per user and per week, with all the corresponding KPIs
# logs_weekly = logs[c(1,2,11:ncol(logs))] %>% group_by(User=logs$User, Week=logs$Week) %>% summarise_each(funs(mean))
logs_weekly = select(logs, User, Week, ends_with("_w")) %>% group_by(User=logs$User, Week=logs$Week) %>% summarise_each(funs(mean))
logs_daily = select(logs, User, Week, Day, ends_with("_d")) %>% group_by(User=logs$User, Week=logs$Week, Day=logs$Day) %>% summarise_each(funs(mean))
logs_weekdaily_avg = select(logs, User, Weekday, ends_with("_d")) %>% group_by(User=logs$User, Weekday=logs$Weekday) %>% summarise_each(funs(mean))

au_logs_weekly_avg = select(logs, Week, ends_with("_w")) %>% group_by(Week=logs$Week) %>%
        summarise_each(funs(mean))
au_logs_daily_avg = select(logs, Week, Day, ends_with("_d")) %>% group_by(User=logs$User, Week=logs$Week, Day=logs$Day) %>%
        summarise_each(funs(mean))
au_logs_weekdaily_avg = select(logs, Weekday, ends_with("_d")) %>% group_by(User=logs$User, Weekday=logs$Weekday) %>%
        summarise_each(funs(mean))

au_logs_weekly_sum = select(logs, Week, ends_with("_w")) %>% group_by(Week=logs$Week) %>%
  summarise_each(funs(sum))
au_logs_daily_sum = select(logs, Week, Day, ends_with("_d")) %>% group_by(User=logs$User, Week=logs$Week, Day=logs$Day) %>%
  summarise_each(funs(sum))
au_logs_weekdaily_sum = select(logs, Weekday, ends_with("_d")) %>% group_by(User=logs$User, Weekday=logs$Weekday) %>%
  summarise_each(funs(sum))


# c(user_stats, all_user_stats) := computeStats(logs, logs_weekly)

user<-"Audrey Auberjonois"

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
        
        h3("metrics"), 
        "means progress: ", textOutput("meanProg"),
        "avg daily consumption: ", textOutput("avgConsoInfo"),
        br()
          
        
               
      ),
    column(9,
       
                       tabsetPanel(type = "tabs",
                                   tabPanel("classic",plotlyOutput('plotSmokwd'), plotlyOutput('plotconso7d'),plotlyOutput('plotProgd')),
                                   tabPanel("week", plotlyOutput("plotAvgDConsoW"), plotlyOutput("plotUsagew")), #, plotlyOutput("plotCorrected")
                                   tabPanel("engagement", plotlyOutput("plotEng2D"), plotlyOutput("plotEng2W")),
                                   tabPanel("all days",plotlyOutput('plotConsod'), plotlyOutput("plot0") ),
                                   tabPanel("analysis", plotlyOutput("plot1"), plotlyOutput("plot2"), plotlyOutput("plot3")
                                    )
                       )

      )
    ),    
    tags$footer(tags$em("Created by Thomas Dufour & Pierre Moreau"))
      ),
      tabItem("all_users",
              fluidRow(
                column(3, align="center",
                       h3("Savings"), 
                       textOutput("AUsaved"), br(),
                       textOutput("AUavgSaved"), br(),
                       "Money saved (L£): ", textOutput("AUmoneySaved"), br(),
                       "Avg Money saved (L£): ", textOutput("AUavgMoneySaved"), br(),
                       br()

                ),
                column(9,

                       tabsetPanel(type = "tabs",
                                   tabPanel("classic",plotlyOutput("AUavgConsow"),plotlyOutput("AUavgProg")),
                                   tabPanel("engagement", plotlyOutput("AUavgEng"))
                        )
  
                )
              )
      )
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
    user_vals$logs_weekly <- logs_weekly %>% filter(User == input$user)
    user_vals$logs_daily <- logs_daily %>% filter(User == input$user)
    user_vals$logs_weekdaily_avg <- logs_weekly %>% filter(User == input$user)
    # user_vals$smoked_computed_in_stat = user_stats$total_smoked[(user_stats$User == user)]
  })
  
  #getting the user info as reactive variable between server and client
  output$userGender<-reactive(as.character(survey$Gender[survey$Name == input$user]))
  output$userAge<-reactive(as.character(paste(survey$Age[survey$Name == input$user],"years old")))
  output$BMI<-reactive(as.character(paste("BMI :",round(survey$weigh[survey$Name == input$user]/((survey$height[survey$Name == input$user]/100)**2))) ))
  
  #calcutlating the difference between the cigs consuption before and now (by integral diference)
  output$saved <- renderText({user_vals$saved})
  
  #and the amount of money saved, in libanese pounds
  output$moneySaved <-renderText({user_vals$saved*3475/20}) 
  
  # output$oldSmoked <-renderText({user_vals$smoked}) 
  # output$newSmoked <-renderText({user_vals$smoked_computed_in_stat}) 
  
  output$currentActivity <- renderText(paste("Active :",{user_vals$current_activity}))
  output$currentEngaged <- renderText(paste("Engaged :",{user_vals$current_engaged}))
  output$meanProg <- renderText(paste({round(mean(user_vals$logs_weekly$Progress_w))}))
  output$avgConsoInfo<- renderText(paste({round(mean(user_vals$logs_daily$Smoked_d))}))
  
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
  
  #render the engament daily
  output$plotEng2D <- renderPlotly({
    df = data.frame(eng = user_vals$logs_daily$Engagement2_d,
                    day = user_vals$logs_daily$Day)
    p1 <- plot_ly(x= df$day, y = df$eng, name="engagement", type = 'scatter', mode="lines",
                  line = list(color = '#00b159', width = 2,shape = "hvh"),
                  fill = 'tozeroy', fillcolor = '#00b159')%>%
    config(displayModeBar = F)%>%
      layout( title = "User's engagement per day", hovermode = 'compare')
    
    
  })
  
  #render the engament weekly
  output$plotEng2W <- renderPlotly({
    df = data.frame(eng = user_vals$logs_weekly$Engagement2_w,
                    week = user_vals$logs_weekly$Week)
    p1 <- plot_ly(x= df$week, y = df$eng, name="engagement", type = 'scatter', mode="lines",
                  line = list(color = '#00b159', width = 2,shape = "hvh"))%>%
      add_lines(x=df$week, y = 0.4, name= "Thereshold",type = 'scatter', mode = 'lines',
                line = list(color = '#d11141', width = 2,shape = "hvh", dash= 'dash'))%>%
      config(displayModeBar = F)%>%
      layout( title = "User's engagement during the weeks", hovermode = 'compare')
  })
  
  #render the comsupton over week day
  output$plotSmokwd <- renderPlotly({
    # df = data.frame(smk = user_vals$logs_weekdaily_avg$Consumption_d,
    #                 weekday = user_vals$logs_weekdaily_avg$Weekday)
    # smk <- user_vals$logs_weekdaily_avg$Consumption_d
    # weekday <- user_vals$logs_weekdaily_avg$Weekday
    # df <- data.frame(smk, weekday)
    
    # df = aggregate(logs_weekdaily_avg$Smoked_d ~ logs_weekdaily_avg$Weekday, user_vals$logs_weekdaily_avg, mean)
    df = aggregate(Consumption_d ~ Weekday, logs[logs$User==input$user,], mean)
    
    # df$Weekday <- factor(df$Weekday, levels= c("lundi", "mardi",
    #                                          "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))
    # 
    # tmp = sort.list(df, method = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))
    # 
    # df = df[order(df$Weekday), ]
    # 
    # p1 <- plot_ly(x= c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"),
    #               y = tmp, name="weekday", type = 'scatter', mode="bar",
    #               line = list(color = '#ffa900', width = 2,shape = "hvh"))%>%
    #   config(displayModeBar = F)%>%
    #   layout( title = "smoke repartition over the weekdays", hovermode = 'compare')
    

    # p <- plot_ly(df, labels = df$weekday, values = df$smk, type = 'pie') %>%
    #   config(displayModeBar = F)%>%
    #   layout(title = 'smoke repartition over the weekdays',
    #          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    # 
    
    p1 <- plot_ly(x=df$Weekday, y=df$Consumption_d, name="Conso", type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
      config(displayModeBar = F)%>%
      layout( title = "smoke repartition over the weekdays", hovermode = 'compare')
    
  })
  
  output$plotconso7d <- renderPlotly({
    df = data.frame(values= tail(user_vals$logs_daily$Consumption_d, n=7))
    
    p1 <- plot_ly(x= c(-7, -6, -5, -4, -3, -2, -1), 
                  y = df$values, name="consumption", type = 'scatter', mode="lines",
                  line = list(color = '#ec0082', width = 2,shape = "hvh"),
                  fill = 'tozeroy', fillcolor = '#ec0082')%>%
      config(displayModeBar = F)%>%
      layout( title = "Consumption evolution over the last 7 days", hovermode = 'compare')
    })
  
  output$plotProgd <- renderPlotly({
    df = data.frame(values = user_vals$logs_weekly$Progress_w)
    p1 <- plot_ly(x= array(1:length(df$values)), 
                  y = df$values, name="progress", type = 'scatter', mode="lines",
                  line = list(color = '#589427', width = 2,shape = "hvh"),
                  fill = 'tozeroy', fillcolor = '#589427')%>%
      config(displayModeBar = F)%>%
      layout( title = "Progress over time", hovermode = 'compare')
  })
  
  output$plotAvgDConsoW <- renderPlotly({
    df = data.frame(values =  user_vals$logs_weekly$Consumption_w/7)
    p1 <- plot_ly(x= array(1:length(df$values)), 
                  y = df$values, name="progress", type = 'scatter', mode="lines",
                  line = list(color = '#00b159', width = 2,shape = "hvh"),
                  fill = 'tozeroy', fillcolor = '#00b159')%>%
      config(displayModeBar = F)%>%
      layout( title = "Avg daily consumption over weeks", hovermode = 'compare')
  })
  
  output$plotUsagew <- renderPlotly({
    df = data.frame(ontime =user_vals$logs_weekly$Ontime_w,
                    cheated = user_vals$logs_weekly$Cheated_w,
                    friend = user_vals$logs_weekly$Friend_w)
    
    p1 <- plot_ly(x= array(1:length(df$ontime)), y=df$ontime, name="OnTime", type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
      add_lines(x= array(1:length(df$cheated)), y=df$cheated, name="Cheated", type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
      add_lines(x= array(1:length(df$friend)), y=df$friend, name="Friend", type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
      config(displayModeBar = F)%>%
      layout( title = "What is ligthing up the lighter", hovermode = 'compare')
  })
  
  # output$plotCorrected <- renderPlotly({
  #   df = data.frame(conso =user_vals$logs_weekly$Consumption_w,
  #                   corrected =  user_vals$logs_weekly$Consumption_w_corrected)
  #   
  #   p1 <- plot_ly(x= array(1:length(df$conso)), y=df$conso, name="Consumption", type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
  #     add_lines(x= array(1:length(df$corrected)), y=df$corrected, name="C. corrected", type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
  #     config(displayModeBar = F)%>%
  #     layout( title = "Correction of consumption", hovermode = 'compare')
  # })
  
  output$plotConsod <- renderPlotly({
    df = data.frame(values = user_vals$logs_daily$Consumption_d )
    p1 <- plot_ly(x= array(1:length(df$values)), 
                  y = df$values, name="consumption", type = 'scatter', mode="lines",
                  line = list(color = '#589427', width = 2,shape = "hvh"))%>%
      config(displayModeBar = F)%>%
      layout( title = "Consumption over time", hovermode = 'compare')
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
    lw = user_vals$logs_weekly %>% filter(Week>0)
    df = data.frame(values = lw$Plan_w, 
                    week = lw$Week) 
    habit = user_vals$habits_d*7
    
    p1 <- plot_ly(x=df$week, y=df$values, name="Plan", type="bar")%>%
      add_lines(x=df$week, y = habit, name= "Habit ",type = 'scatter', mode = 'lines',
                    line = list( width = 3, dash='dash')
                ) %>%
    config(displayModeBar = F)%>%
    layout( title = "How did the consuption evolved", hovermode = 'compare')
    }) 

  #Hey there's the all users part now!
  output$AUsaved <- renderText(paste("cig saved: ", {sum(logs_weekly$Plan_w[logs_weekly$Week!=0])- all_user_stats$total_smoked}))
  output$AUavgSaved <- renderText(paste("avg saving: ", {round((sum(logs_weekly$Plan_w[logs_weekly$Week!=0])- all_user_stats$total_smoked)/length(unique(logs_weekly$User)))}))
  output$AUmoneySaved <- renderText(paste({round((sum(logs_weekly$Plan_w[logs_weekly$Week!=0])- all_user_stats$total_smoked)*3475/20)}))
  output$AUavgMoneySaved <- renderText(paste({round((sum(logs_weekly$Plan_w[logs_weekly$Week!=0])- all_user_stats$total_smoked)/length(unique(logs_weekly$User)))*3475/20}))
  
  
  output$AUavgConsow <- renderPlotly({
    df <- aggregate(Consumption_d ~ Weekday, logs_weekdaily_avg, sum)
    
    p1 <- plot_ly(x=df$Weekday, y=df$Consumption_d, name="Conso", type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
      config(displayModeBar = F)%>%
      layout( title = "Consumption during the week", hovermode = 'compare')
  })
  output$AUavgProg <- renderPlotly({
    df = aggregate(Progress_w ~ Week, logs_weekly, mean)
    
    p1 <- plot_ly(x=df$Week, y=df$Progress_w, name="Progress", type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
      config(displayModeBar = F)%>%
      layout( title = "Evolution of mean Progress", hovermode = 'compare')
  })
  output$AUavgEng <- renderPlotly({
    df = aggregate(Engagement_w ~ Week, logs_weekly, mean)
    
    p1 <- plot_ly(x=df$Week, y=df$Engagement_w, name="Progress", type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
      config(displayModeBar = F)%>%
      layout( title = "Evolution of mean Progress", hovermode = 'compare')
  })
  
}
shinyApp(ui, server)