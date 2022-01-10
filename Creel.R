################################################################################################################
#            FROM "RAW" ACCESS SURVEY TABLE TO "CLEAN" ALLSURVEYZIPCODESSTACKED TABLE
# Original Script: GatherZipcodes2.0.R
################################################################################################################
library(readxl)
library(tidyr)
library(xlsx)
library(plyr)
library(dplyr)
library(stats)
library(lubridate)
library(odbc)
library(RODBC)
library(DBI)
library(rgdal)
################################################################################################################
#           CONNECT TO CREEL DATABASE AND EXTRACT SURVEY QUERY TABLE 
#
################################################################################################################


filename.db <- paste0("W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Creel Survey related/2017 Creel Survey/2017 Creel survey database all years.accdb")

con <- dbConnect(odbc::odbc(),
                 .connection_string = paste("driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                                            paste0("DBQ=", filename.db), "Uid=Admin;Pwd=;", sep = ";"),
                 timezone = "America/New_York")

#df.queryold <- dbGetQuery(con, "SELECT AllSurveyZipCodesStackedBest.LoopNumber, AllSurveyZipCodesStackedBest.[Survey Number], AllSurveyZipCodesStackedBest.SurveyDate, AllSurveyZipCodesStackedBest.ZipNum, AllSurveyZipCodesStackedBest.ZipCode,
#AllSurveyZipCodesStackedBest.Field1, MasterZipCodelist.Town, MasterZipCodelist.State, MasterZipCodelist.Zip_Code
 #          FROM AllSurveyZipCodesStackedBest INNER JOIN MasterZipCodelist ON (AllSurveyZipCodesStackedBest.ZipCode = MasterZipCodelist.Zip_Code) AND (AllSurveyZipCodesStackedBest.ZipCode = MasterZipCodelist.Zip_Code);")

df.query <- dbReadTable(con, "SurveyZip Query")
##could also read in tables and query within using dplyr

#join and innerjoin, leftjoin
dbListTables(con)


################################################################################################################
#            TOWN/STATE OF ORIGIN ANGLER COUNT,  AND APPLICATION OF EXPANSION FACTOR 
#
################################################################################################################



#read in zip code stack queried with master zip code list. From access. second argument is sheet name
#surveystack <- read_excel("W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Creel Survey related/2017 Creel Survey/AllSurveyZipCodesStackedBestQuery.xlsx", "AllSurveyZipCodesStackedBest_Qu")
#add a year column using the Survey date. 
df.query$year <- year(df.query$SurveyDate)
#create a 'Townstate' column, which will act as an important joining key in ArcMap
#create a duplicate, all caps version of this column 
df.query$Townstate <- paste(df.query$Town, df.query$State, sep = ", ")  %>% 
  as.factor()

df.query <- df.query %>% group_by(df.query$Townstate, df.query$year) %>% 
  summarise(count = n()) 
names(df.query) <- c("Townstate", "SurveyYear", "Count") 


df.query <- df.query[,1:3]
df.query$expansion <- NA
df.query$expansion[df.query$SurveyYear == 2017] <-  round(df.query$Count[df.query$SurveyYear == 2017]*6.919)
df.query$expansion[is.na(df.query$expansion)] <-  round(df.query$Count[is.na(df.query$expansion)]*11.2357) 
df.query <- select(df.query, -3) %>% 
  spread(value = "expansion", key = "SurveyYear")

df.query[is.na(df.query)] <- 0
df.query$Townstate <- as.character(df.query$Townstate)

df.query$Total <- rowSums(df.query[,2:4], na.rm = FALSE, dims = 1)

df.query$TOWNST <- df.query$Townstate
df.query$TOWNST <- toupper(df.query$TOWNST)

                      
dbDisconnect(con)
rm(con)

################################################################################################################
#            JOIN EXPANSION TABLE TO MATOWNS.shp ATTRIBUTE TABLE 
#
################################################################################################################



#read in shpfile and attribute table
matown <- readOGR("W:/WatershedJAH/EQStaff/Aquatic Biology/GIS/Creel Survey/2017Update/MATOWNS_polyforunion.shp")

?readOGR
#join attribute table and dataframe
matown@data <- left_join(matown@data, df.query, by=c("TownST" = "TOWNST"))

#create a new shpfile with joined attribute table 
writeOGR(matown, "W:/WatershedJAH/EQStaff/Aquatic Biology/GIS/Creel Survey/2017Update/MATOWNS_Rtest2.shp", layer="MATOWNS_R", driver="ESRI Shapefile" )











