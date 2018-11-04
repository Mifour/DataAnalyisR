# create by Thomas Dufour & Pierre Moreau, oct. 2018
library(shiny)
library(shinydashboard)
library(xlsx)
library(fastDummies)
library(readxl)
library(stats)
library(stringr)

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
        #numericInput("user", "User's number to view:", 1),
        #selectInput(inputId = 'user', 'User', choices = c("Renaud Courbis","Paola Ange","InÃ¨s Delaunay","Constantin Bain","Ã‰tienne Toussaint","Emma Milhaud","Marcel Jauffret","Romuald Chappuis","Audrey Auberjonois","Vivien TrouvÃ©","Joseph Toussaint","William Beauregard",'Francis Boucher',"Ã‰douard Charbonneau","Jordan Lafaille","BÃ©nÃ©dicte Brosseau","Abel Sharpe","Marc Gaumont","Davy Gaudreau","Astrid Boudier","Christine Guillaume","Baptiste Mallet","Dominique Barnier","Jean-Charles Delaplace","FÃ©lix Beauvau","JoÃ«l Brazier","Christiane Lacan","Ange Besnard","Davy Tomas","Laurette Boulanger","Richard Reverdin","Blaise Neri","JÃ©rÃ©my Durand","RÃ©mi Dubost","Wilfried Piaget","Matthieu Bertillon"),
        box(h3(verbatimTextOutput(survey$Name[survey$Name == user])),br(),
            verbatimTextOutput(survey$Gender[survey$Name == user]), ",",
            verbatimTextOutput(survey$Age[survey$Name == user]), " years old",br(),
            "BMI: ", verbatimTextOutput((survey$weigh[survey$Name ==user]/((survey$height[survey$Name ==user]/100)**2))) ),
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
    return(survey)
  }
  
  
  processSurvey(unique(survey$Name))
  processLogs(unique(logs$User))
  
  logs<-logs[order(logs$User),]
  surveyDummies <- (fastDummies::dummy_cols(survey[,c(-1,-2,-3,-5,-7,-10,-11,-57,-58,-108,-109)], remove_first_dummy = TRUE))[,-1:-158]
  
  # The currently selected tab from the first box
  output$tabset1Selected <- renderText({
    input$tabset1
  })
  output$user<-reactive(as.character(input$user))
  output$hist<-renderPlot({ hist(rnorm(input$n)) }) 
  user <- reactive({input$user})
  
  values <- reactiveValues()
  getValues <- reactive({ values$ind <- which(user[,1]%in%survey$Name)}) 
  output$userName <- renderPrint({
  getValues() })
  
  #output$userName<-renderText({survey$Name[user]})
  output$userGender<-renderText({survey$Gender[user]})
  output$userAge<-renderText({survey$Age[user]})
  output$BMI<-renderText({ (survey$weigh[user]/((survey$height[user]/100)**2)) })
  
  
  
}
shinyApp(ui, server)