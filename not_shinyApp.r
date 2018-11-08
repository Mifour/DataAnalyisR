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
  # habits <-length(logs$Time[logs$User == input$user & logs$Type== "Behaviour"])/7
  # This is a KPI computed in the Absolute sense (not relative to weeks) on a user basis
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
              CASE WHEN Activity_w > 0.3 AND Engagement_w < 0.7 THEN 1 ELSE 0 END AS Engaged_w
             FROM logs")
  
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

# Keep 1 line per user and per week, with all the corresponding KPIs
logs_weekly = aggregate(logs, list(User=logs$User, Week=logs$Week), mean)[c(1,2,11:ncol(logs))] 

user<-"Audrey Auberjonois"
