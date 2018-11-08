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
  
  return(logs)
}

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
user<-"Audrey Auberjonois"



ui<- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard",
               icon = icon("dashboard")),
      menuItem("Github", icon = icon("github", class = "fab", lib = "font-awesome"),
               href= "https://github.com/Mifour/DataAnalyisR")
    )
  ),
  body <- dashboardBody(
    fluidRow(
      column(3, align="center",
        selectInput("user", "User", choices = survey$Name, selected = survey$Name ),
        h3(verbatimTextOutput("user")),br(),
            verbatimTextOutput("userGender"),
            verbatimTextOutput("userAge"), " years old",br(),
            "BMI: ", verbatimTextOutput("BMI"), br() ,
        
          h3("Savings"), br(),
          "Cigs saved: ", textOutput("saved"), br(),
          "Money saved (L£): ", textOutput("moneySaved"),
          br(),
        
          h3("Current Activities"), br(),
          "Active: ", 1, br(),
          "Engaged: ", 1,
          br(),
       
          h3("Feature usage"), br(),
          plotOutput("distPlot0")
        
               
      ),
    column(9,
       
      plotlyOutput("distPlot1"),
       
      plotOutput("distPlot2"),
       
      plotOutput("distPlot3")
       
      )
    ),    
    tags$footer(tags$em("Created by Thomas Dufour & Pierre Moreau"))
  )
  
  
)

server = function(input, output) {
  #manipulating the data
  
  
  logs<-logs[order(logs$User),]
  surveyDummies <- (fastDummies::dummy_cols(survey[,c(-1,-2,-3,-5,-7,-10,-11,-57,-58,-108,-109)], remove_first_dummy = TRUE))[,-1:-158]
  
  
  output$user<-reactive(as.character(input$user))
  
  #output$userName<-renderText({survey$Name[user]})
  output$userGender<-reactive(as.character(survey$Gender[survey$Name == input$user]))
  output$userAge<-reactive(as.character(survey$Age[survey$Name == input$user]))
  output$BMI<-reactive(as.character(round(survey$weigh[survey$Name == input$user]/((survey$height[survey$Name == input$user]/100)**2))) )
  
  tmp <-reactive({as.data.frame(logDate = logs$Time[logs$User == input$user & logs$Type== "Cheated"])%>%group_by(logDate) %>% summarise(no_logs = length(logDate)) })
  #tmp <- tmp %>%group_by(logDate) %>% summarise(no_logs = length(logDate))
  #output$distPlot1 <- renderPlot(autoplot(ts(tmp)) + labs(title="Cheated over time") )

  #output$distPlot1 <- reactivePlot(function(){
   # autoplot(ts(tmp)) + labs(title="Cheated over time")
  #})
  
  #habits <-reactive(as.character(length(logs$Time[logs$User == input$user & logs$Type== "Behaviour"])/7 )) 
  #smoked <- reactive(as.data.frame(logDate = logs$Time[(logs$User == input$user & logs$Type== "On time") |(logs$User == input$user & logs$Type== "Cheated" )]) )
  output$saved <- renderText({
    habits <-length(logs$Time[logs$User == input$user & logs$Type== "Behaviour"])/7
    smoked <-data.frame(logDate = logs$Time[(logs$User == input$user & logs$Type== "On time") |(logs$User == input$user & logs$Type== "Cheated" )] )
    saved <-habits*as.numeric(max(as.Date.factor(smoked$logDate))-min(as.Date.factor(smoked$logDate)))-length(smoked) 
  })
  output$moneySaved <-renderText({ 
    habits <-length(logs$Time[logs$User == input$user & logs$Type== "Behaviour"])/7
    smoked <-data.frame(logDate = logs$Time[(logs$User == input$user & logs$Type== "On time") |(logs$User == input$user & logs$Type== "Cheated" )] )
    saved <-habits*as.numeric(max(as.Date.factor(smoked$logDate))-min(as.Date.factor(smoked$logDate)))-length(smoked)
    moneySaved <- saved *3.475/20 
  }) 
  
  output$distPlot1 <- renderPlotly({ 
      df = logs[logs$User == input$user] 
      df = df[,c("User", 'Time')]
      df = aggregate(df$User, by= list(df$Time), length)
      colnames(df) = c("Day", "nbLog")
      
      plot_ly(x=df$Day, y=df$nbLog, name="test", type="bar")
  }) 
  
  
  # 
  # #C = corr(#skipped, #cheated)
  # skipped_W =length(logs$Time[logs$User == input$user & logs$Type== "Skipped" & logs$Week == input$Week])
  # cheated_W =length(logs$Time[logs$User == input$user & logs$Type== "Cheated" & logs$Week == input$Week])
  # autoSkipped_W = length(logs$Time[logs$User == input$user & logs$Type== "autoSkipped" & logs$Week == input$Week])
  # onTime_W = length(logs$Time[logs$User == input$user & logs$Type== "onTime" & logs$Week == input$Week])
  # 
  # smoked_W = (onTime_W+cheated_W)
  # plan_W = (onTime_W+skipped_W+autoSkipped_W)
  # activity = smoked_W/plan_W
  # 
  # actif_W = 0
  # if (activity >0.3) {
  #   actif_W = 1
  # }
  # 
  # tmp <- data.frame(skipped_W, cheated_W)
  # C_W <- cor(tmp)
  # #V = (#cheated + #skipped)/(#skipped + #onTime + #autoSkipped +1)
  # V_W <- ((skipped + cheated)/(skipped + autoskipped + onTime +1))
  # Engagement_W <- C_W*V_W
  # if (Engagement_W >0.7 & Active_W ==1){
  #   Engaged_W = 1
  # }
  # else {
  #   Engaged_W =0
  # }
  
  
}
shinyApp(ui, server)