library(xlsx)
library(tidyr)
library(plyr)
library(dplyr)
library(stats)
library(lubridate)

getwd()
setwd("W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Creel Survey related/2017 Creel Survey")

#instead of creating and exporting an excel file every time this stack needs to happen, connect to db
#bring in stacked excel file from access
RAWsurveystack <- read.xlsx("20180209AllSurveyZipCodesStackedNewQueryforArcGIS.xlsx","AllSurveyZipCodesStackedNew_Que")
#rename columns
RAWsurveystack$year <- year(RAWsurveystack$SurveyDate)
RAWsurveystack$Townstate <- paste(RAWsurveystack$Town, RAWsurveystack$State, sep = ", ")  %>% 
  as.factor()
names(groupedzipsALL) <- c("Townstate", "SurveyYear", "Count", "2017_Expansion")                       

groupedzipsALL <- RAWsurveystack %>% group_by(RAWsurveystack$Townstate, RAWsurveystack$year) %>% 
  summarise(count = n()) 

groupedzipsALL$expansion <- round(groupedzipsALL$Count*6.919)
groupedzipsALL$expansion <- round(groupedzipsALL$Count*6.919)
#2011 and 2012 expansion factor = 11.2357
str(zipall)

zipall <- groupedzipsALL[,1:3]
zipall$expansion <- NA

zipall$expansion[zipall$SurveyYear == 2017] <-  round(zipall$Count[zipall$SurveyYear == 2017]*6.919)
zipall$expansion[is.na(zipall$expansion)] <-  round(zipall$Count[is.na(zipall$expansion)]*11.2357) 
zipallGIS <- select(zipall, -3) %>% 
  spread(value = "expansion", key = "SurveyYear")
write.xlsx(zipallGIS, "zipallGIS.xlsx")







zipall$expansion <-  round(zipall$Count*11.2357)








Expansionfunctest <- function(x) {
 a <- (x[,3])
    b <- 6.919  
    c <- 11.24
  if (x[2]=="2017")
    out <- a * b
  else if (x[2]=="2011")
    out <- a * c
  else if (x[2]=="2012")
    out <- a * c
  return(out)
} 
TestexpansionFactor <- Expansionfunctest(groupedzipsALL)

g <- apply(groupedzipsALL, 2, Expansionfunctest)
groupedzipsALL[,3] <- t(g)
groupedzipsALL


Expansionfunc("2017")


Expansionfunctest2 <- function(x) {
  a <- (x[,3])
  if (x[2]==2017)
    out <- a * 6.919
  else if (x[2]==2011)
    out <- a * 11.24
  else if (x[2]==2012)
    out <- a * 11.24
  return(out)
} 
Expansiontest2obj <- Expansionfunctest2(groupedzipsALL)




ls()
rm(list = ls())
RAWsurveystack2011 <- read.xlsx("20180209AllSurveyZipCodesStackedNewQueryforArcGIS.xlsx","2011")
RAWsurveystack2012 <- read.xlsx("20180209AllSurveyZipCodesStackedNewQueryforArcGIS.xlsx","2012")
RAWsurveystack2017 <- read.xlsx("20180209AllSurveyZipCodesStackedNewQueryforArcGIS.xlsx","2017")
View(RAWsurveystack)

??Count

unique_zips2017 <- lapply(RAWsurveystack2017, unique)
zips2017 <- RAWsurveystack2017[,c(1:4, 6)]

zips2017MA <- zips2017[zips2017$State == "MA", ]

zips2017outofstate <- zips2017[zips2017$State != "MA", ]

sapply(zips2017MA, class)
unique(zips2017MA$Town)
nrow(zips2017)

by(zips2017$State == "MA", zips2017[, "Town"], summary)
#kind of worked. How do I report which state it is in? 
lapply(zips2017MA, count, "Town")
aggregate(zips2017MA$Town, by=zips2017MA["Town"], FUN = count)


outofstatezipstable <- table(zips2017outofstate$Town)
write.xlsx(outofstatezipstable, "outofstatezipstabletowncounts.xlsx")
library(forcats)

str(zips2017MA)
count(zips2017, "Town")

groupedzips <- zips2017 %>% group_by(zips2017$Townstate, zips2017$year) %>% 
  summarise(count = n())



                     
                  
                        
                        
                        
                        
                        
                        
                        
                      
                      zips2017$Townstate %>% tally()

zips2017$Townstate <- paste(zips2017$Town, zips2017$State, sep = ", ")  %>% 
  as.factor()

??elem
unique
tapply(zips2017$Town,zips2017$State, summary)
tapply(zips2017MA$Town, zips2017MA$State, summary)
#Close with this function. Try other apply functions. 
Un
??tapply
??aggregate
??summaryby
??nrow
