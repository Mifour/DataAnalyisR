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

survey = read.xlsx("surveydataece.xlsx",1)
survey<-survey[1:36,]
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
processLogs <- function(logs){
  logs = Unaccent(logs)
  logs = str_replace_all(logs, 'Z', 'e')
  logs = str_replace(logs, "ftienne", "Etienne")
  logs = str_replace(logs, "flie", "Elie")
  logs = str_replace(logs, "In?s", "Ines")
  logs = str_replace(logs, "fdouard", "Edouard")
  return(logs)
}

processSurvey <- function(survey){
  survey = str_replace_all(survey, "é", "e")
  survey = str_replace_all(survey, "É", "E")
  survey = str_replace_all(survey, "ë", "e")
  survey = str_replace_all(survey, "è", "e")
  survey = str_replace_all(survey, "Ã©", "e")
  survey = str_replace_all(survey, "Ã¨", "e")
  survey = str_replace_all(survey, "Ã‰", "E")
  survey = str_replace_all(survey, "Ã«", "e")
  return(survey)
}


survey$Name <-processSurvey(unique(survey$Name))
logs$User <-processLogs(logs$User)
user<-"William Beauregard"

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
        selectInput("user", "User", choices = sort(survey$Name), selected = sort(survey$Name) ),
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
  
  
  
  #C = corr(#skipped, #cheated)
  skipped_W =length(logs$Time[logs$User == input$user & logs$Type== "Skipped" & logs$weekNum == input$weekNum])
  cheated_W =length(logs$Time[logs$User == input$user & logs$Type== "Cheated" & logs$weekNum == input$weekNum])
  autoSkipped_W = length(logs$Time[logs$User == input$user & logs$Type== "autoSkipped" & logs$weekNum == input$weekNum])
  onTime_W = length(logs$Time[logs$User == input$user & logs$Type== "onTime" & logs$weekNum == input$weekNum])
  
  smoked_W = (onTime_W+cheated_W)
  plan_W = (onTime_W+skipped_W+autoSkipped_W)
  activity = smoked_W/plan_W
  
  actif_W = 0
  if (activity >0.3) {
    actif_W = 1
  }
  
  tmp <- data.frame(skipped_W, cheated_W)
  C_W <- cor(tmp)
  #V = (#cheated + #skipped)/(#skipped + #onTime + #autoSkipped +1)
  V_W <- ((skipped + cheated)/(skipped + autoskipped + onTime +1))
  Engagement_W <- C_W*V_W
  if (Engagement_W >0.7 & Active_W ==1){
    Engaged_W = 1
  }
  else {
    Engaged_W =0
  }
  
  
}
shinyApp(ui, server)