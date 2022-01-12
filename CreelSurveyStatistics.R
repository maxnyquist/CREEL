################################### HEADER ###################################
#   TITLE: CreelSurveyStatistics.R
#   DESCRIPTION:This script calculates all necessary totals and estimations for the Creel Survey and the associated Report. The estimates from 
#   this script also serve other planning purposes, including calculating survey effort and building a survey calendar. 
#   AUTHOR(S): Max Nyquist
#   DATE LAST UPDATED: 1/12/2022
#   R VERSION 4.0.3
##############################################################################.

### LOAD PACKAGES ####
pkg <- c("tidyverse", "magrittr", "lubridate","RODBC", "DBI", "odbc")
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
  Creel_AgentResult <- dbReadTable(con, Id(schema = schema, table = 'tbl_Creel_AgentResult'))
  Creel_Agents <- dbReadTable(con, Id(schema = schema, table = 'tbl_Creel_Agents'))
  Creel_FishCaught <- dbReadTable(con, Id(schema = schema, table = 'tbl_Creel_FishCaught'))
  Creel_SurveyResult <- dbReadTable(con, Id(schema = schema, table = 'tbl_Creel_SurveyResult'))
  df.Survey_Agent_Result <- left_join(Creel_SurveyResult, Creel_AgentResult, by = "LoopNumber")
  df.Fish_Survey_Agent <- left_join(Creel_FishCaught, df.Survey_Agent_Result, by = "SurveyNumber")
  
  
  ### Always disconnect and rm connection when done with db
  dbDisconnect(con)
  rm(con)
  
  
  

### SCRIPT BEGIN ####