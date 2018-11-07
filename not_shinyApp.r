# create by Thomas Dufour & Pierre Moreau, oct. 2018

# setwd("C:/Users/cmoim/Dropbox/Cours/ING5/Data Analytics/0_PROJECT/DataAnalyisR")

library(xlsx)
library(fastDummies)
library(readxl)
library(stats)
library(stringr)
library(ggplot2)
library(dplyr)
library(plotly)
library(stringr)
library(sqldf)


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
processLogsUsernames <- function(logs){
  logs = Unaccent(logs)
  logs = str_replace_all(logs, 'Z', 'e')
  logs = str_replace(logs, "ftienne", "Etienne")
  logs = str_replace(logs, "flie", "Elie")
  logs = str_replace(logs, "In?s", "Ines")
  logs = str_replace(logs, "fdouard", "Edouard")
  
  return(logs)
}

processLogs <- function(logs){
  # Process user names
  logs$User <-processLogsUsernames(logs$User)
  
  # Remove the null columns  (keep only the first 5 columns)
  logs <- logs[c(1:5)]
  
  # Compute Day number = datedif("day", Time, min(Time) of user)
  # Since I love SQL and I already did the computation in SQL in Tableau Software, I'm using the SQL package
  logs = sqldf("select l.*, d.firstDate
              from logs l
              left join (
              -- I compute the minimim date for each user
              -- and add 1 day such that the first behaviour day will be the -1 day
                  select User, date(min(Time), '+1 day') as firstDate from logs group by User
              )d on l.User = d.User")

  # We compute the day difference between the log input and the first log input
  logs$Day <- as.numeric(as.Date.factor(logs$Time) - as.Date.factor(logs$firstDate))

  # We remove the logs where day = -1 (the behaviour week starts always 1 day too early)
  logs = logs[logs$Day >= 0,]

  # Compute week number = floor(Day#/7)
  logs$Week <- floor(logs$Day/7)
  
  return(logs)
}

processSurveyUsernames <- function(survey){
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
user<-"William Beauregard"
