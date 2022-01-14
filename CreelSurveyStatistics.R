################################### HEADER ###################################
#   TITLE: CreelSurveyStatistics.R
#   DESCRIPTION:This script calculates all necessary totals and estimations for the Creel Survey and the associated Report. The estimates from 
#   this script also serve other planning purposes, including calculating survey effort and building a survey calendar. 
#   AUTHOR(S): Max Nyquist
#   DATE LAST UPDATED: 1/12/2022
#   R VERSION 4.0.3
##############################################################################.

### LOAD PACKAGES ####
pkg <- c("tidyverse", "magrittr", "lubridate","RODBC", "DBI", "odbc", "bizdays")
sapply(pkg, library, character.only = TRUE)

### SET OPTIONS/ENV VARIABLES ####

### Eliminate Scientific Notation
  options(scipen = 999)

### SOURCE DATA/FUNCTIONS/FILES ####

R_Config <- read.csv("//env.govt.state.ma.us/enterprise/DCR-WestBoylston-WKGRP/WatershedJAH/EQStaff/WaterQualityMonitoring/R-Shared/Configs/R_Config.csv", header = TRUE)
config <- as.character(R_Config$CONFIG_VALUE)

### FETCH CACHED DATA FROM WAVE RDS FILES ####
### script currently does not utilize .rds files 
  # datadir <- config[1]
  # ### Make a list of all the .rds files using full path
  # rds_files <- list.files(datadir,full.names = TRUE ,pattern = "\\.rds$")
  # rds_files # Take a look at list of files
  # ### Select which rds files needed for this script
  # rds_in <- c(3,4,7:9)
  # ### subset rds files (if you want all of them then skip rds_in and the following line)
  # rds_files <- rds_files[rds_in]
  # ### create an object that contains all of the rds files
  # data <- lapply(rds_files, readRDS)
  # ### Make a list of the df names by eliminating extension from files
  # df_names <- gsub(".rds", "", list.files(datadir, pattern = "\\.rds$"))
  # df_names <- df_names[rds_in]
  # # name each df in the data object appropriately
  # names(data) <- df_names
  # ### Extract each element of the data object into the global environment
  # list2env(data ,.GlobalEnv)
  # ### Remove data
  # rm(data)

### CONNECT TO A FRONT-END DATABASE ####
  ###  Error: nanodbc/nanodbc.cpp:1021: IM002: [Microsoft][ODBC Driver Manager] Data source name not found and no default driver specified
  ### Set DB
  db <- config[3]
  ### Connect to Database 
  con <- dbConnect(odbc::odbc(),
                   .connection_string = paste("driver={Microsoft Access Driver (*.mdb)}",
                                              paste0("DBQ=", db), "Uid=Admin;Pwd=;", sep = ";"),
                   timezone = "America/New_York")
  ### See the tables 
  tables <- dbListTables(con)  
  ### Fetch an entire table (avoid using SQL queries - just pull entire tables and subset in R)
  tbl <- dbReadTable(con, "tblParameters")
  ### Always disconnect and rm connection when done with db
  dbDisconnect(con)
  rm(con)

### CONNECT TO WQ DATABASE ####
### Set DB
  database <- 'DCR_DWSP'
  schema <- 'Wachusett'
  tz <- 'America/New_York'
  ### Connect to Database 
  con <- dbConnect(odbc::odbc(), 'DCR_DWSP', timezone = tz)
  ### See the tables
  tables <- dbListTables(con, schema_name = schema) %>% print()
### Creel Tables   
  df.agentresult <- dbReadTable(con, Id(schema = schema, table = 'tbl_Creel_AgentResult'))
  df.nameagents <- dbReadTable(con, Id(schema = schema, table = 'tbl_Creel_Agents'))
  df.fishcaught <- dbReadTable(con, Id(schema = schema, table = 'tbl_Creel_FishCaught'))
  df.survey <- dbReadTable(con, Id(schema = schema, table = 'tbl_Creel_SurveyResult'))
  df.survey_agent <- left_join(Creel_SurveyResult, Creel_AgentResult, by = "LoopNumber")
  df.fish_survey_agent <- left_join(Creel_FishCaught, df.Survey_Agent_Result, by = "SurveyNumber")
  
  
  ### Always disconnect and rm connection when done with db
  dbDisconnect(con)
  rm(con)
  
  
  

### SCRIPT BEGIN ####

  
### Begin Estimated Angler Hours ####
### rename for testing  
df.agentresult <- Creel_AgentResult  
#strptime(df.agentresult$SunUpET, format = "%H:%M:%S", tz = "EST")
  
### test as_hms. Gives different Daylight hours value than strptime. Correct seems to be in Estimated Daily Angler Hours.xlsx. possible as_hms converts to seconds, does not account for 60 sec = 1 min, 60 min = 1 hr. ? strptime knows how to handle time.      
# df.agentresult <- Creel_AgentResult  %>% 
#   mutate(SunUpET = as_hms(SunUpET), SunDownET = as_hms(SunDownET), LoopStartTimeET = as_hms(LoopStartTimeET), LoopEndTimeET = as_hms(LoopEndTimeET)) %>% 
#   mutate(DaylightHours = as_hms(SunDownET - SunUpET))

### Daylight Hours   
df.agentresult <- Creel_AgentResult  %>% 
  mutate(SunUpET = strptime(SunUpET, format = "%H:%M:%S", tz = "EST"), SunDownET = strptime(SunDownET, format = "%H:%M:%S", tz = "EST"), LoopStartTimeET = strptime(LoopStartTimeET, format = "%H:%M:%S", tz = "EST"), LoopEndTimeET = strptime(LoopEndTimeET, format = "%H:%M:%S", tz = "EST")) %>% 
  mutate(DaylightHours = as.numeric(difftime(SunDownET, SunUpET))) %>% 
  mutate(EstAngler_Hrs_loop = DaylightHours*Angler_Count)

### Estimated Daily Angler Hours by Survey Loop - need this for Hours by Month and Year 
df.dailyhours <- df.agentresult %>% 
  group_by(LoopDate, Type) %>% 
  summarise(DailyEst_AnglerHrs=mean(EstAngler_Hrs_loop))

### Estimated Daily Angler Hours by Month and Year - need this for effort calculations  
df.monthlyhours <- df.dailyhours %>% 
  group_by(year(LoopDate),month(LoopDate),Type) %>% 
  summarise(MonthlyEst_AnglerHrs = sum(DailyEst_AnglerHrs))

### Get total number of survey days, AM and PM loops combined. Obsolete after df.survey and df.totals created. Keep for now
# df.surveydaze <- df.agentresult %>% 
#   group_by(year(LoopDate), month(LoopDate), Type) %>% 
#   distinct(LoopDate) %>%
#   summarise(SurveyDays = n())
# 
### Df.survey creates Angler count and Angler survey totals by Date  
df.survey <- df.agentresult %>% 
  group_by(LoopDate, year(LoopDate), month(LoopDate), Type) %>%
  summarise(Angler_Count_sum = sum(Angler_Count), Angler_Surveyed_sum = sum(Anglers_Surveyed)) 
### df.totals further groups Angler count and Angler survey totals, while also counting number of distinct survey days    
df.totals <- df.survey %>% 
  group_by(year(LoopDate), month(LoopDate), Type) %>% 
  summarise(SurveyDays = n_distinct(LoopDate), Angler_Count_sum = sum(Angler_Count_sum), Angler_Surveyed_sum = sum(Angler_Surveyed_sum)) %>% 
  rename("year" = "year(LoopDate)", "month" = "month(LoopDate)" )

### Days per Month Builder ####
### This should work with any new incoming data. Do not want to update year information 
years.survey <- unique(df.totals$`year(LoopDate)`)
months.survey <- unique(df.totals$`month(LoopDate)`)

maxdays <- as.data.frame(days_in_month(months.survey))
maxdays$months <- row.names(maxdays)
maxdays$months <-match(x$months, month.abb)
#months.survey <- paste0("0",unique(df.totals$`month(LoopDate)`))
calendar <- merge(years.survey, months.survey)
#calendar <- c(t(outer(years.survey, months.survey, paste, sep = "-")))
#calendar <- outer(years.survey, months.survey, FUN = paste, sep = "-")
#cal.df <- as.data.frame(calendar)
#x$months <- as.character(x$months)
calendar <-  left_join(calendar, maxdays, by = c("y" = "months")) %>% 
  rename("year" ="x","month" = "y" , "end.day"= "days_in_month(months.survey)")%>% 
  mutate(start.date = paste(year, month, 1, sep = "-")) %>% 
  mutate(end.date = paste(year, month, end.day, sep = "-")) 
calendar$start.date <- ymd(calendar$start.date)
calendar$end.date <- ymd(calendar$end.date)

#calendar2$end.date <- paste(x, y, days_in_month(months.survey), sep = "-")
# (start.date = paste(x, y, 1, sep = "-"),
# start <- ymd(paste(years.survey[1], months.survey[1], "1", sep = "-"))
# end <-  ymd(paste(years.survey[2], months.survey[1], "30", sep = "-")) 
# first <- as.Date(cut(start, "month"))
# last <- as.Date(cut(first +31, "month")) - 1
# 
### Financial argument must be FALSE. Financial calendars do not consider ending business day 
creel.week.calendar <- create.calendar(name = "creel.week.calendar",weekdays = c("saturday", "sunday"), financial = FALSE)
creel.wknd.calendar <- create.calendar(name = "creel.wknd.calendar", weekdays = c("monday", "tuesday", "wednesday", "thursday", "friday"), financial = FALSE)
#holidays()
calendar$weekdays <- bizdays(from = calendar$start.date, to = calendar$end.date, cal = "creel.week.calendar")
calendar$weekenddays <- bizdays(from = calendar$start.date, to = calendar$end.date, cal = "creel.wknd.calendar" )
# 
# v1 <- seq(from = ymd(paste(years.survey[1], months.survey[1], "1", sep = "-")), to = ymd(as.character(paste(years.survey[1], months.survey[1], "30", sep = "-"))), by='day')
# v2 <- seq(from = ymd(paste(years.survey[2], months.survey[1], "1", sep = "-")), to = ymd(as.character(paste(years.survey[2], months.survey[1], "30", sep = "-"))), by='day')
# 
# v1 <- seq(from = ymd(paste(years.survey[1], months.survey[1], "1", sep = "-")), to = ymd(as.character(paste(years.survey[1], months.survey[1], "30", sep = "-"))), by='day')
# v2 <- seq(from = ymd(paste(years.survey[2], months.survey[1], "1", sep = "-")), to = ymd(as.character(paste(years.survey[2], months.survey[1], "30", sep = "-"))), by='day')
# 
# ### this works with a short sequence
# working_days <- sum(wday(v1)>1 & wday(v1)<7)
# off_days <- sum(wday(v1)==1 | wday(v1)==7)
# 
df.test <- left_join(df.totals, calendar) %>%
  select(year, month, Type, SurveyDays, Angler_Count_sum , Angler_Surveyed_sum, weekdays, weekenddays)
### gather or spread this table, match up Type and weekend/weekday total days  
### testing w filter code 
# test <- df.survey %>% 
#   filter(month(LoopDate) == "4" & Type == "Weekend")

### Plotting and Fish Shapes  ####
install.packages("fishualize")
library(devtools)
devtools::install_github("nschiett/fishualize", force = TRUE)
library(fishualize)
library(fishualize)
fishapes()

