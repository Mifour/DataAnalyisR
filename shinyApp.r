# create by Thomas Dufour & Pierre Moreau, oct. 2018
library(shiny)
library(shinydashboard)
library(xlsx)
library(fastDummies)
library(readxl)
library(stats)
library(stringr)

survey = read.xlsx("surveydataece.xlsx",1)
survey<-survey[1:36,]
logs = read.csv2("logs.csv")

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
        box(h3(verbatimTextOutput("user")),br(),
            verbatimTextOutput("userGender"), ",",
            verbatimTextOutput("userAge"), " years old",br(),
            "BMI: ", verbatimTextOutput("BMI") ),
        box(
          h3("Savings"), br(),
          "Cigs saved: ", 528, br(),
          "Money saved: ", 91
          ),
        box(
          h3("Current Activities"), br(),
          "Active: ", 1, br(),
          "Engaged: ", 1
          ),
        box(
          h3("Feature usage"), br(),
          plotOutput("distPlot0")
        )
        )       
      ),
    column(9,
       wellPanel(
         plotOutput("distPlot1")
       ),
       wellPanel(
         plotOutput("distPlot2")
       ),
       wellPanel(
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
  
  
  
}
shinyApp(ui, server)