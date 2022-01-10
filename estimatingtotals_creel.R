################################### HEADER ###################################
#   TITLE: estimatingtotals_creel.R
#   DESCRIPTION:  This script is a recreation of "2017 Table for Estimating Totals.xlsx" workbook.
#   Historically, this workbook was used to complete estimations for angler use and fish catch rates, 
#   that are reported in the Wachusett Reservoir Creel Survey. The calculations in the workbook have been
#   written into this script in order to make results reproducible with less risk for data loss.
#   AUTHOR(S): Max W. Nyquist
#   DATE LAST UPDATED: 1/21/2019
#   R VERSION 32 bit R Portable
##############################################################################.


### LOAD PACKAGES ####
pkg <- c("tidyverse", "magrittr", "lubridate","RODBC", "DBI", "odbc", "colorspace", "readxl", "Rcpp")
sapply(pkg, library, character.only = TRUE)
#install.packages("Rcpp")

#DescTools and devtools and readr may need to be added to the above pkg assignment
### SET OPTIONS/ENV VARIABLES ####

### Eliminate Scientific Notation
options(scipen = 999)

getwd()
###Database connection and file import####
creel.db <- paste0("C:/CreelDatabase/2017 Creel survey database all years1.mdb")
con <- dbConnect(odbc::odbc(),
                 .connection_string = paste("driver={Microsoft Access Driver (*.mdb)}",
                                            paste0("DBQ=", creel.db), "Uid=Admin;Pwd=;", sep = ";")
                 )

###1/22/2020. Change made directly in Creel database. 4/10/2011, Friday, AM Loop, Bruce Fant. Loop number changed from 4 to 3. Friday changed to Sunday. Changes made in Agent sheet 
#including timezone set the times incorrectly
#timezone = "America/New_York"
dbListTables(con)
#create data.frames of relevant access tbls. 
#Fish Caught Query is a combination of Fish Caught, Agent Sheet, and Survey Table. Consider recreating in R
fishquery.df <- dbReadTable(con, "Fish Caught Query")
fish.df <- dbReadTable(con, "Fish Caught")
survey.df <- dbReadTable(con, "Survey Table")
agent.df <- dbReadTable(con, "Agent Sheet")
namesagents <- dbReadTable(con, "Survey Agent")


####SCRIPT BEGIN####
#fix dates in df to create correct datetimes 
survey.df <- survey.df %>% 
  mutate(., Time.Started.Fishing = as.POSIXct(paste0(SurveyDate, format(survey.df$Time.Started.Fishing, "%H:%M:%S")))) %>% 
  mutate(., Interview.Time = as.POSIXct(paste0(SurveyDate, format(survey.df$Interview.Time, "%H:%M:%S")))) %>% 
  mutate(., Type = ifelse(.$DayoftheWeek %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) %>%
  mutate(AnglerHours = difftime(survey.df$Interview.Time, survey.df$Time.Started.Fishing, units = "hours")) %>% 
  rename(Date = SurveyDate, LoopNo = LoopNumber, Day = DayoftheWeek, SurveyNo = Survey.Number, AnglerNo = X..Anglers, LinesNo = X..lines, FishCaughtNo = X..Fish.caught) 
#Agent Table has incorrect number of anglers interviewed. Use Survey Table for this value. 
agent.df <- agent.df %>% 
  mutate(., Sun.Down = as.POSIXct(paste0(Loop.Date, format(agent.df$Sun.Down, "%H:%M:%S")))) %>% 
  mutate(., Sun.Up = as.POSIXct(paste0(Loop.Date, format(agent.df$Sun.Up, "%H:%M:%S")))) %>% 
  mutate(., Loop.Start.Time = as.POSIXct(paste0(Loop.Date, format(agent.df$Loop.Start.Time, "%H:%M:%S")))) %>% 
  mutate(., Loop.End.Time = as.POSIXct(paste0(Loop.Date, format(agent.df$Loop.End.Time, "%H:%M:%S")))) %>% 
  mutate(daylighthours = difftime(Sun.Down, Sun.Up, units = "hours")) %>% 
  mutate(loophours = difftime(Loop.End.Time, Loop.Start.Time)) %>% 
  left_join(namesagents, by = c("Lead.Survey.Agent" = "ID")) %>% 
  left_join(namesagents, by = c("X2nd.Survey.Agent" = "ID")) %>% 
  rename(Date = Loop.Date, LoopNo = Loop.Number, Day = Day.of.Week, AgentsNo = X..Agents, Agent1 = Survey.Agent.x, 
          Agent2 = Survey.Agent.y,  NosurveyNotThere = Not.surveyed..not.there, NosurveyNoGo = Not.surveyed..didn.t.go, 
         NosurveyNoCoop = Not.surveyed.no.cooperate, Nosurvey30min = Not.surveyed...30.min, 
         NosurveyIllegal = Not.surveyed..illegal, NosurveyTooMany = Not.surveyed..too.many.anglers) %>% 
    select(-(Lead.Survey.Agent), -(X2nd.Survey.Agent))
  
anglerNo <- survey.df %>% 
  group_by(LoopNo) %>% 
  summarize(anglersinterview = sum(AnglerNo)) 
#flagging to see discrepancies between survey angler count and agent angler count. Big issue. 
flagging.df <- left_join(agent.df, anglerNo, by = "LoopNo") %>% 
  mutate(flags = (anglersinterview - Anglers.Surveyed))


dbDisconnect(con)
rm(test)

#Need to get actual angler hours 



#####Expansion Factor Tables#####
#Catch Rate expansion factor by Month with Year
#To find Angler trips expansion Factor:
#Estimated # of Angler Trips/# of anglers interviewed
#Estimated # of Angler Trips = Total Est. Angler Hours/Average Completed trip (hours)
#Total Est. Angler Hours = (#Days in Month/#Survey Days)*Total Daily Est. Angler Hours
#Average Completed trip (hours) = Total completed trip angler survey hours/#Anglers w completed trips 

#Could join both tables right away

#Could create two summary tables with group_by and then combine later
#This summary creates a different number of anglers surveyed than excel workbook
test.agent <- agent.df %>% 
  group_by(month(Date), year(Date), Type) %>% 
  summarize(anglercount = sum(Angler.Count), anglersinterview = sum(Anglers.Surveyed))
test.agent <- agent.df %>% 
  group_by(Date) %>% 
  summarize(anglercount = sum(Angler.Count), anglersinterview = sum(Anglers.Surveyed))


 
 
test.survey <- survey.df %>% 
  group_by(month(Date), year(Date), Type) %>% 
  summarize(anglersinterview = sum(AnglerNo), anglerhours = sum(AnglerHours))


test.survey <- survey.df %>% 
  group_by(LoopNo) %>% 
  summarize(anglersinterview = sum(AnglerNo)) 
 
 
catchexp <- as.data.frame(totals[3])
catchexp <- catchexp[c(1, 26)]
catchexp$year <- ""
catchexp$year[c(3:11)] <- 2011
catchexp$year[c(17:25)] <- 2012
catchexp$year[c(31:39)] <- 2017
colnames(catchexp) <- c("month", "Factor", "Year")
catchexp$Year <- as.integer(catchexp$Year)
catchexp <- drop_na(catchexp)
catchexp <- catchexp[!c(catchexp$month == 'Month'), ]
catchexp$Factor <- as.numeric(catchexp$Factor)

#Angler trips expansion factor by Type, Month and Year
expanse <- as.data.frame(totals[1])
expanse <- expanse[, c(1,2,14, 17)]
expanse <- na.omit(expanse)
colnames(expanse) <- c("Month", "Type", "Factor", "Year")
expanse$Year <- as.integer(format(expanse$Year, format = "%Y"))
expanse$Factor <- as.numeric(expanse$Factor)

#Harvest Rate 
#From excel sheet: Harvest rate = (Total Reported Kept/Total Reported Caught)

#These values could be calculated instead of assigned, by using values from the fish dataframe
###These harvest rates are for Lake Trout only
smbharvrate <- data.frame("Year" = c(2011, 2012, 2017), "Rate" =  c(0.0860927152317881, 0.141538461538462, 0.0784313725490196))

###############################################################################################################################
#&df$`Kept.Released`== "Kept"&
#Annual Harvest and Maximum Sustainable Yield with Angler trips expansion factor 
#Equation for calculating gramWeight can be found in LTtagging_lengthweight.R
#####Take Fish.Length..inches. limit out depending on regulation testing
#&fish$Fish.Length..inches.>=20
dfmsy <- fish[c(fish$`Fish.Species`=="Smallmouth Bass (SMB)"),]
dfmsy$Month <- months(dfmsy$SurveyDate)
dfmsy$Year <- as.integer(format(dfmsy$SurveyDate, format = "%Y"))
dfmsy$mmLength <- (dfmsy$`Fish.Length..inches.`/0.0393701)
#dfmsy$gramWeight <- ((0.0211*(dfmsy$mmLength*dfmsy$mmLength))-(14.504*(dfmsy$mmLength))+2933.9)
#testing new allometric equation from LTtagging_lengthweight.R
#####log(W)=-14.7+3.47*log(L)####Log transformed equation. e^-14.7 gives constant below(0.0000004113259)
dfmsy$gramWeight <- ((coeffa)*(dfmsy$mmLength^coeffb))
dfmsy$poundWeight <- dfmsy$gramWeight*0.00220462

###Cannot do this with above table as the Survey.Number has been duplicated 
dfmsy<- left_join(dfmsy,expanse, by = c("Month" = "Month",  "Year" = "Year", "Type" = "Type"), copy = F)
dfmsy$estanglers <- dfmsy$X..Anglers*dfmsy$Factor
#Log transformation attempt...confusing
# dfmsy$loggramWeight <- ""
# dfmsy$loggramWeight <- (-14.70388+3.47*log10(dfmsy$mmLength))
# dfmsy$loggramWeight2 <- log10(dfmsy$loggramWeight)
# dfmsy$test <- 10^dfmsy$logpoundWeight
# dfmsy$poundWeight <- ((0.03*(dfmsy$`Fish.Length..inches`*dfmsy$`Fish.Length..inches`))-(0.8122*(dfmsy$`Fish.Length..inches.`))+6.4682)
# dfmsy$gramtopound <- dfmsy$gramWeight*0.00220462
# [dfmsy$`Kept.Released`== "Kept",]

# 
# dfmsy1 <- dfmsy %>%
#   group_by(Year) %>%
#   summarise(meanlength = mean(`Fish.Length..inches.`), meanweight = mean(poundWeight))

######
# dfmsy1$mmLength <- (dfmsy1$avglength_in/0.0393701)
# dfmsy1$gramWeight <- ((0.0211*(dfmsy1$mmLength*dfmsy1$mmLength))-(14.504*(dfmsy1$mmLength))+2933.9)
# dfmsy1$poundWeight <- dfmsy1$gramWeight*0.00220462
#into a new df, sum poundWeight by month and by year and by Type(weekday or weekend)
#Then, appropriate expansion factor can be applied to weekend days and weekdays, and added
# #MSY with grams and Angler trips expansion Factor
# dfgrams <- aggregate(gramWeight ~ Month + Year + Type, dfmsy, sum )
# dfgrams <- left_join(dfgrams,expanse, by = c("Month" = "Month", "Type" = "Type", "Year" = "Year"), copy = F)
# dfgrams$estimateWeight <- dfgrams$gramWeight*dfgrams$Factor
# dfgrams <- aggregate(estimateWeight~Year, dfgrams, sum)
# dfgrams$pounds <- dfgrams$estimateWeight*0.00220462
# dfgrams$lbsperacre <- (dfgrams$estimateWeight/4057)*0.00220462
# dfgrams$kgperha <- ((dfgrams$estimateWeight/1000)/(4057*0.404686))
#"Type" = "Type",
# #MSY with pounds and Angler trips Expansion Factor
# #MSY with pounds and Angler trips Expansion Factor
# dflbs <- aggregate(poundWeight ~ Month + Year + Type, dfmsy, sum )
# dflbs <- left_join(dflbs,expanse, by = c("Month" = "Month", "Type" = "Type", "Year" = "Year"), copy = F)
# dflbs$estimateWeight <- dflbs$poundWeight*dflbs$Factor
# dflbs <- aggregate(estimateWeight~Year, dflbs, sum)
# dflbs$lbsperacre <- dflbs$estimateWeight/4057
# dflbs$kgperha <- ((dflbs$estimateWeight*0.453592)/(4057*0.404686))
######
#######with Catch Rate expansion factor
#######format 1 (length limits)####

dflbs <- dfmsy %>%
  group_by(Month, Year) %>%
  dplyr::summarise(count = n(), meanlength = mean(`Fish.Length..inches.`), meanweight = mean(poundWeight))


dftotal <- dfmsy %>%
  group_by(Month, Year) %>%
  dplyr::summarise(count = n(), totallength = sum(`Fish.Length..inches.`), totalweight = sum(poundWeight))


# #
# dflbs <- left_join(dflbs,catchexp, by = c("Month" = "month",  "Year" = "Year"), copy = F)
# dflbs$Estcaught <- dflbs$count*dflbs$Factor

#dflbs$fishpang <- dflbs$Estcaught/dflbs$estanglers
#dflbs <- filter(dflbs, fishpang <= 2)
# dflbstest <- dflbs %>% 
#   group_by(Year) %>% 
#   summarise(Estcaught = sum(Estcaught))
# dflbstest <- left_join(dflbstest, ltltharvrate, by = "Year")
# #dflbstest$Yield <- dflbstest$sumlbs/dflbstest$Estcaught
# dflbstest$Estharv <- dflbstest$Estcaught*dflbstest$Rate
# dflbstest <- left_join(dflbstest, dfmsy1, by ="Year")
# dflbstest$Yield <- dflbstest$Estharv*dflbstest$meanweight
# dflbstest$lbsperacre <- dflbstest$Yield/4057
####format 2 (creel limit 2 fish)#####
#, meanlength = mean (`Fish.Length..inches.`), meanweight = mean(poundWeight)
#with Error: n() should only be called in a data context in summarise(count = n()), correct with dplyr::summarise
# # dflbs <- dfmsy %>% 
#   group_by(Survey.Number, Month, Year, X..Anglers) %>% 
#   dplyr::summarise(count = n(), meanlength = mean(`Fish.Length..inches.`), meanweight = mean(poundWeight))
# dflbs$fishpang <- dflbs$count/dflbs$X..Anglers
# #####Turn this filter on/off depending on regulation testing#####
# #dflbs <- dflbs %>% 
#   filter(!(Kept.Released=="Kept"))
# 

#if fishpang<=2 when Kept.Released == Kept 

#Streamline with dplyr piping
dftotal <- left_join(dftotal,catchexp, by = c("Year" = "Year", "Month" = "month"), copy = F)
dftotal$Estcaught <- dftotal$count*dftotal$Factor



dflbs <- left_join(dflbs,catchexp, by = c("Year" = "Year", "Month" = "month"), copy = F)

dflbs$Estcaught <- dflbs$count*dflbs$Factor


####TESTING
# dfest <- dftotal %>% 
#   group_by(Year) %>% 
#   dplyr::summarise(totalcaught = sum(count), meanlength = mean(totallength), estcaught = sum(estcaught), meanweight = mean(totalweight), yield = sum(totalweight))
# 
# dfest2 <- dftotal %>% 
#   group_by(Year) %>% 
#   dplyr::summarise(totalcaught = sum(count), estcaught = sum(estcaught), totallength = sum(totallength),  totalweight = sum(totalweight))


###original
# dflbs2 <- dfmsy[dfmsy$Kept.Released=="Kept", ] %>%
#   group_by(Survey.Number, Month, Year, X..Anglers, Kept.Released) %>%
#   dplyr::summarise(count = n(), meanlength = mean (`Fish.Length..inches.`), meanweight = mean(poundWeight))
# 
# ##test version
dflbs2 <- dfmsy[dfmsy$Kept.Released=="Kept", ] %>%
  group_by(Survey.Number, Month, Year, X..Anglers, Kept.Released) %>%
  dplyr::summarise(count = n(), meanlength = sum (`Fish.Length..inches.`), meanweight = sum(poundWeight))


dflbs2$fishpang <-  dflbs2$count/dflbs2$X..Anglers
#####Turn this filter on/off depending on regulation testing#####
#dflbs2 <- filter(dflbs2, fishpang <= 2)

#if_else(dflbs$Kept.Released == "Kept", 0) 
#insert creel limit of 2 fish per day 
#dfmsy2 <- dflbs %>% 
  #group_by(Year) %>% 
  #summarise(meanlength = mean(`Fish.Length..inches.`), meanweight = mean(poundWeight))
dflbstest <- dflbs %>% 
  group_by(Year) %>% 
  dplyr::summarise(Estcaught = sum(Estcaught))
dflbstest <- left_join(dflbstest, smbharvrate, by = "Year")

# dflbstest <- dftotal %>% 
#   group_by(Year) %>% 
#   dplyr::summarise(Estcaught = sum(Estcaught))
# dflbstest <- left_join(dflbstest, ltharvrate, by = "Year")
# 



#dflbstest$Yield <- dflbstest$sumlbs/dflbstest$Estcaught
dflbstest$Estharv <- dflbstest$Estcaught*dflbstest$Rate
###testing
# dflbstest2 <- dflbs2 %>% 
#   group_by(Year) %>% 
#   mutate(meanlength = (meanlength/count), meanweight = (meanweight/count)) %>% 
#   dplyr::summarise(length = meanlength, weight = meanweight)
#####THIS ONE WORKS
dflbstest2 <- dflbs2 %>% 
  group_by(Year) %>% 
  dplyr::summarise(length = sum(meanlength)/sum(count), weight = sum(meanweight)/sum(count))

#mutate(meanlength = (meanlength/count), meanweight = (meanweight/count)) %>% 
  
 # dplyr::summarise(length = (meanlength/count), weight = (meanweight/count))


###original
# dflbstest2 <- dflbs2 %>% 
#   group_by(Year) %>% 
#   dplyr::summarise(length = mean(meanlength), weight = mean(meanweight))
# 



dflbstest <- left_join(dflbstest, dflbstest2, by="Year")
#dflbstest <- left_join(dflbstest, dfmsy2, by ="Year")
dflbstest$Yield <- dflbstest$Estharv*dflbstest$weight
dflbstest$lbsperacre <- dflbstest$Yield/4057
#test version by adding 2 at the end
yieldtablesmb <- tibble("Year"= c(dflbstest$Year, "Totals"), 
                  "Estimate Caught" = c(dflbstest$Estcaught, sum(dflbstest$Estcaught)),
                  "Mean Length" = c(dflbstest$length, mean(dflbstest$length)), 
                  "Mean Weight" = c(dflbstest$weight, mean(dflbstest$weight)),
                  "Harvest Rate" = c(dflbstest$Rate, mean(dflbstest$Rate)),
                  "Estimate Harvest" = c(dflbstest$Estharv, sum(dflbstest$Estharv)), 
                  "Yield (pounds)" = c(dflbstest$Yield, sum(dflbstest$Yield)),
                  "Total Yield (pounds/acre)" = c(dflbstest$lbsperacre, mean(dflbstest$lbsperacre))
)

write_csv(yieldtable2, paste0("W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Creel Survey related/2017 Creel Survey/", "yieldtableupdate.csv"))



# 
# dflbs$estimateWeight <- dflbs$poundWeight*dflbs$Factor
# dflbs <- aggregate(estimateWeight~Year, dflbs, sum)
# dflbs$lbsperacre <- dflbs$estimateWeight/4057
# dflbs$kgperha <- ((dflbs$estimateWeight*0.453592)/(4057*0.404686))
# 
# # ###with Catch Rate expansion factor
# dftest <- aggregate(poundWeight ~ Month + Year, df, sum )
# dftest$Year <- as.integer(dftest$Year)
# dftest <- left_join(dftest,catchexp, by = c("Month" = "month", "Year" = "Year"), copy = F)
# dftest$estimateWeight <- dftest$poundWeight*dftest$Factor
# dftest2 <- aggregate(estimateWeight~Year, dftest, sum)
# dftest2$lbsperacre <- dftest2$estimateWeight/4057
# dftest2$kgperha <- ((dftest2$estimateWeight*0.453592)/(4057*0.404686))
# ###############################################################################################################################
# #Angler Trips Analysis
#
#This script will provide the number of surveys where Lake Trout of the specified size were caught
#Multiply by the angler trips expansion factor 
#Kept.Released, X..Anglers
dflt <- fish[c(fish$`Fish.Species`=="Lake Trout (LT)"),]
dflt <- dflt[c(dflt$`Fish.Length..inches.`>= 18),]
dflt$Month <- months(dflt$SurveyDate)
dflt$Year <- as.integer(year(dflt$SurveyDate))
dfltc <- dflt %>%
group_by(Survey.Number, Month, Year) %>%
dplyr::summarise(count = n())%>%
filter(count > 2)
dfltccang<- dfltc %>% 
  group_by(Survey.Number, Month, Year, X..Anglers) %>% 
  dplyr::summarise(count = n())#%>% 
dfltccang$fishpang <- dfltccang$count/dfltccang$`X..Anglers`
dfltccang <- dfltccang %>% 
 group_by(Survey.Number, Month, Year, fishpang) %>% 
  filter(fishpang > 2)
#dftest <- dfltccaught[c(duplicated(dfltccaught))]
#dfltcctest <- left_join(dfltccang, expanse, by = c("Year" = "Year", "Month"= "Month"))
#dfltcctest$estanglers <- dfltcctest$X..Anglers*dfltcctest$Factor
dfltcctest <- left_join(dfltccang, catchexp, by = c( "Month" = "month", "Year" = "Year"))
dfltcctest$fishest <- ((dfltcctest$fishpang))*dfltcctest$Factor
# dfltccang2 <- dfltcctest %>% 
#   group_by(Year) %>% 
#   dplyr::summarise(sum = sum(fishest))
# # dfltccang1 <- dfltccang %>% 
#   group_by(Month, Year) %>% 
#   dplyr::summarise(sum = sum()) 
# dfltccang1 <- left_join(dfltccang1,catchexp,  by = c( "Month" = "month", "Year" = "Year"))
# dfltccang1$estcatch <- dfltccang1$sum*dfltccang1$Factor
# dfltccang1 <- dfltccang1 %>% 
#   group_by(Year) %>% 
#   dplyr::summarise(sum = sum(estcatch))
dfltcctest$fishest <- ((dfltcctest$fishpang))*dfltcctest$Factor
dfltcctest <- left_join(dfltcctest, smbharvrate, by = "Year")
dfltcctest$fishharvest <- dfltcctest$fishest*dfltcctest$Rate
dfltccang$fish_angler <- dfltccang$fishest/dfltccang$estanglers
dfltcctest <- dfltccang[c(dfltccang$fish_angler>=2.0), ]
dfltcc <- left_join(dfltcc, catchexp, by = c("Month"="month", "Year"="Year"))
dfltcc$EstFish <- dfltcc$Factor*dfltcc$count
dfsumc <- dfltcc %>% 
  group_by(Year) %>% 
  dplyr::summarise(total = sum(count), obs = n(), est = sum(EstFish)) 
#dfsumc$harv_est <- dfsumc$est*ltharvrate$Rate
dfsumc <- left_join(dfsumc, smbharvrate, by = c("Year" = "Year"))
dfsumc$Harvest <- dfsumc$est*dfsumc$Rate
dfsumc <- tibble("Year"= c(dfsumc$Year, "Totals"), 
                  "Surveys with >2 under 18in. Fish Caught" = c(dfsumc$obs, sum(dfsumc$obs)),
                  "Fish Caught" = c(dfsumc$total, sum(dfsumc$total)), 
                  "Fish Caught Estimate" = c(dfsumc$est, sum(dfsumc$est)),
                 "Annual Harvest Rate" = c(dfsumc$Rate, mean(dfsumc$Rate)),
                 "Fish Harvest Estimate" = c(dfsumc$Harvest, sum(dfsumc$Harvest))
                 )
#############
write_csv(dfsumc, paste0("W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Creel Survey related/2017 Creel Survey/", "harvestestimates18limit.csv"))

dfltk <- dflt[c(dflt$`Kept.Released`== "Kept"),]
dfltk <- dfltk#[c(dfltk$`Fish.Length..inches.`< 19),]
#Find and count the surveys with more than 2 fish caught from the dataframe with only data from fish smaller than 18
dfltk$Month <- months(dfltk$SurveyDate)
dfltk$Year <- as.integer(year(dfltk$SurveyDate))
dfltkept <- dfltk %>% 
  group_by(Survey.Number, Month, Year, Type,  X..Anglers) %>% 
  dplyr::summarise(count = n())%>% 
  filter(count > 2) 
sum(dfltkept$X..Anglers)
#Fish.Length..inches.,

dfltkept$fish_angler <- dfltkept$count/dfltkept$X..Anglers
dfltkeptovertwo <- dfltkept#[c(dfltkept$fish_angler>2.0), ]
dfltkeptovertwo <- left_join(dfltkeptovertwo, expanse, by = c("Year" = "Year", "Month"= "Month", "Type" = "Type"))
dfltkeptovertwo$estanglers <- dfltkeptovertwo$X..Anglers*dfltkeptovertwo$Factor
dfltkeptovertwo <- left_join(dfltkeptovertwo, catchexp, by = c( "Month" = "month", "Year" = "Year"))
dfltkeptovertwo$caughtfishest <- (dfltkeptovertwo$fish_angler)*dfltkeptovertwo$Factor.y
dfltkeptovertwo <- left_join(dfltkeptovertwo, smbharvrate, by = "Year")
dfltkeptovertwo$harvfishest <- (dfltkeptovertwo$caughtfishest)*dfltkeptovertwo$Rate
sum(dfltkeptovertwo$estanglers)
sum(dfltkeptovertwo$harvfishest)

dfltkept <- left_join(dfltkept, catchexp, by = c("Month"="month", "Year"="Year"))
dfltkept$EstFish <- dfltkept$Factor*dfltkept$count
dfsumk <- dfltkept %>% 
  group_by(Year) %>% 
  dplyr::summarise(total = sum(count), obs = n(), est = sum(EstFish)) 

dfsumk <- tibble("Year"= c(dfsumk$Year, "Totals"), 
                   "Surveys with >2 Fish Kept" = c(dfsumk$obs, sum(dfsumk$obs)),
                   "Fish Kept" = c(dfsumk$total, sum(dfsumk$total)), 
                   "Fish Kept Estimate" = c(dfsumk$est, sum(dfsumk$est))
)


test <- bind_rows(dfsum18, dfsum18k, dfsum20, dfsum20k, dfsumc, dfsumk)
############
#Lake Trout 18" Regulation analysis
#Select all fish caught smaller than 18"
dflt18 <- dflt[c(dflt$`Fish.Length..inches.`< 18),]
#Find and count the surveys with more than 2 fish caught from the dataframe with only data from fish smaller than 18
dflt18$Month <- months(dflt18$SurveyDate)
dflt18$Year <- as.integer(year(dflt18$SurveyDate))
dflt18caught <- dflt18 %>% 
  group_by(Survey.Number, Month, Year) %>% 
  dplyr::summarise(count = n())#%>% 
  filter(count > 2) 
dflt18caught <- left_join(dflt18caught, catchexp, by = c("Month"="month", "Year"="Year"))
dflt18caught <- left_join(dflt18caught, smbharvrate, by = c("Year" = "Year"))
dflt18caught$EstFish <- dflt18caught$Factor*dflt18caught$count
dflt18caught$Harvest <- dflt18caught$EstFish*dflt18caught$Rate
dfsum18 <- dflt18caught %>% 
  group_by(Year) %>% 
  dplyr::summarise(total = sum(count), obs = n(), est = sum(EstFish), kept = sum(Harvest)) 
  
dfsum18 <- tibble("Year"= c(dfsum18$Year, "Totals"), 
                 "Surveys with >2 Fish Caught" = c(dfsum18$obs, sum(dfsum18$obs)),
                 "Fish <18 in. Caught" = c(dfsum18$total, sum(dfsum18$total)), 
                 "Fish Caught Estimate" = c(dfsum18$est, sum(dfsum18$est)),
                 "Fish Harvest Estimate" = c(dfsum18$kept, sum(dfsum18$kept))
                 )

dflt18k <- dflt[c(dflt$`Kept.Released`== "Kept"&dflt$`Fish.Length..inches.`< 18),]
#Find and count the surveys with more than 2 fish caught from the dataframe with only data from fish smaller than 18
dflt18k$Month <- months(dflt18k$SurveyDate)
dflt18k$Year <- as.integer(year(dflt18k$SurveyDate))
dflt18kept <- dflt18k %>% 
  group_by(Survey.Number, Month, Year, X..Anglers) %>% 
  dplyr::summarise(count = n())%>% 
  filter(count > 2) 
dflt18kept <- left_join(dflt18kept, catchexp, by = c("Month"="month", "Year"="Year"))
dflt18kept$EstFish <- dflt18kept$Factor*dflt18kept$count
dfsum18k <- dflt18kept %>% 
  group_by(Year) %>% 
  dplyr::summarise(total = sum(count), obs = n(), est = sum(EstFish)) 
#dfsum18k <- add_case(dfsum18k, Year = "2012", total = 0, est = 0, obs = 0, .after = 1)
dfsum18k <- tibble("Year"= c(dfsum18k$Year, "Totals"), 
                  "Surveys with >2 Fish Kept" = c(dfsum18k$obs, sum(dfsum18k$obs)),
                  "Fish <18 in. Kept" = c(dfsum18k$total, sum(dfsum18k$total)), 
                  "Fish Kept Estimate" = c(dfsum18k$est, sum(dfsum18k$est))
)

dfsumlist <- list(dfsumc, dfsumk, dfsum18, dfsum18k, dfsum20, dfsum20k)
dfsumvec <- c('dfsumc', 'dfsumk', 'dfsum18', 'dfsum18k', 'dfsum20', 'dfsum20k')
sapply(1:length(dfsumlist),
       function(i) write_csv(dfsumlist[[i]], paste0(dfsumvec[i], '.csv')))
########
#####USE BELOW CODE FOR ESTIMATING HARVEST AT DIFFERENT LENGTH LIMITS#################
#Month, Year, Factor, Kept.Released, Type
#Lake Trout 20" Regulation analysis
#No. of surveys where more than 1 20" Lake Trout was caught = 114, kept = 47
dflt20 <- dflt[c(dflt$`Fish.Length..inches.`>= 20),]
#Find and count the surveys with more than 2 fish caught from the dataframe with only data from fish smaller than 20
dflt20$Month <- months(dflt20$SurveyDate)
dflt20$Year <- as.integer(year(dflt20$SurveyDate))
dflt20caught <- dflt20 %>% 
  group_by(Survey.Number, Month, Year, X..Anglers) %>% 
  dplyr::summarise(count = n())%>% 
  filter(count > 2) 
dflt20caught <- left_join(dflt20caught, catchexp, by = c("Month"="month", "Year"="Year"))
dflt20caught <- left_join(dflt20caught, smbharvrate, by = c("Year" = "Year"))
dflt20caught$fishpang <- dflt20caught$count/dflt20caught$X..Anglers 
#dflt20caught <- filter(dflt20caught, fishpang > 2)
dflt20caught$EstFish <- dflt20caught$Factor*dflt20caught$count
dflt20caught$Harvest <- dflt20caught$EstFish*dflt20caught$Rate
dfsum20 <- dflt20caught %>% 
  group_by(Year) %>% 
  dplyr::summarise(total = sum(count), obs = n(), est = sum(EstFish), kept = sum(Harvest)) 

dfsum20 <- tibble("Year"= c(dfsum20$Year, "Totals"), 
                  "Surveys with Fish Caught" = c(dfsum20$obs, sum(dfsum20$obs)),
                  "Fish >=20 in. Caught" = c(dfsum20$total, sum(dfsum20$total)), 
                  "Fish Caught Estimate" = c(dfsum20$est, sum(dfsum20$est)),
                  "Fish Harvest Estimate" = c(dfsum20$kept, sum(dfsum20$kept))
)





dflt20k <- dflt[c(dflt$Kept.Released=="Kept"& dflt$`Fish.Length..inches.`< 20),]
#Find and count the surveys with more than 2 fish caught from the dataframe with only data from fish smaller than 20
dflt20k$Month <- months(dflt20k$SurveyDate)
dflt20k$Year <- as.integer(year(dflt20k$SurveyDate))
dflt20kept <- dflt20k %>% 
  group_by(Survey.Number, Month, Year) %>% 
  dplyr::summarise(count = n())#%>% 
  filter(count > 2) 
dflt20kept <- left_join(dflt20kept, catchexp, by = c("Month"="month", "Year"="Year"))
dflt20kept$EstFish <- dflt20kept$Factor*dflt20kept$count
dfsum20k <- dflt20kept %>% 
  group_by(Year) %>% 
  dplyr::summarise(total = sum(count), obs = n(), est = sum(EstFish)) 

dfsum20k <- tibble("Year"= c(dfsum20k$Year, "Totals"), 
                  "Surveys with >2 Fish Kept" = c(dfsum20k$obs, sum(dfsum20k$obs)),
                  "Fish <20 in. Kept" = c(dfsum20k$total, sum(dfsum20k$total)), 
                  "Fish Kept Estimate" = c(dfsum20k$est, sum(dfsum20k$est))
)



all <- bind_cols(dfsumc, dfsumk, dfsum18, dfsum18k, dfsum20, dfsum20k)
all <- all[, -c(6, 10, 15,19, 24)]
write_csv(all, paste0("W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Creel Survey related/2017 Creel Survey/", "allLTharvestestimates.csv"))
# 
# 
# 
# dflt20 <- dflt[c(dflt$`Fish.Length..inches.`< 20),]
# dflt20$Month <- months(dflt20$SurveyDate)
# dflt20$Year <- as.character(year(dflt20$SurveyDate))
# dflt20 <- dflt20 %>% 
#   group_by(Survey.Number, SurveyDate, Type) %>% 
#   summarise(count = n()) %>% 
#   filter(count >= 2)
# 
# 
# 
# 
# 
# survtest <- aggregate(Survey.Number ~  Year, dflt18, length)
# survgt2k <- aggregate(Survey.Number ~  Year, dflt18, length)
# survgt2k18 <- aggregate(Survey.Number ~  Year, dflt18, length)
# test <- aggregate(EstFish~Year, dflt18, sum)
# sum(test$EstFish)
# 
# 
# 


# dflt20 <- left_join(dflt20, expanse, by = c("Month"="Month", "Year"="Year", "Type"="Type"))
# dflt20$EstFish <- dflt20$Factor*dflt20$count
# 
# 
# 


#Fish Survival Analysis
#df$Kept.Released=="Kept"&

#Use script below to distinguish caught vs. kept
#
#Lake Trout Catch data for regulations#
#This script will provide the number of Lake Trout caught under the specified size
#multiply by the catch rate expansion factor 
#dfcat <- df[c(df$`Fish.Species`=="Lake Trout (LT)"),]
dfcat2 <- dflt
dfcat2$month <- months(dfcat2$SurveyDate)
dfcat2$year <- year(dfcat2$SurveyDate)
dfcat2 <- dfcat2[c(dfcat2$`Fish.Length..inches.`< 20),]
dfcat2 <- dfcat2 %>% 
  group_by(Survey.Number, month, year) %>% 
  summarise(count = n()) %>% 
  filter(count > 2)
testcaught <- aggregate(count ~  year, dfcat2, sum )
sum(testcaught$count)

dfcat18$month <- months(dfcat18$SurveyDate)
dfcat18$year <- year(dfcat18$SurveyDate)
# dfcat3 <- dfcat2 
dfcat18 <- dfcat18 %>% 
  group_by(month, year) %>% 
  summarise(count = n()) #%>% 
 filter()

dfcat18 <-  left_join(dfcat18,catchexp, by = c("month" = "month", "year" = "Year"))
dfcat18$estcaught <- dfcat18$count*dfcat18$Factor 
dfcat18 <- left_join(dfcat18, ltharvrate, by = c("year" = "Year"))
# dfcat18$Harvest <- dfcat18$estcaught*dfcat18$Rate  
testcaught <- aggregate(count ~  year, dfcat2, sum )
testcaught2 <- left_join(testcaught2, catchexp, by = c("year"="Year"))
#testcaught2$Harvest <- testcaught2$count*testcaught2$Factor





dfcat20 <- dflt[c(dflt$`Fish.Length..inches.`< 20),]
dfcat20$month <- months(dfcat20$SurveyDate)
dfcat20$year <- year(dfcat20$SurveyDate)
dfcat20 <- dfcat20 %>% 
  group_by(month, year, Kept.Released) %>% 
  summarise(count = n()) 


dfcat20 <-  left_join(dfcat20,catchexp, by = c("month" = "month", "year" = "Year"))
dfcat20$estcaught <- dfcat20$count*dfcat20$Factor 
dfcat20 <- left_join(dfcat20, ltharvrate, by = c("year" = "Year"))
dfcat20$Harvest <- dfcat20$estcaught*dfcat20$Rate  

caught20 <- aggregate(estcaught ~  year+Kept.Released, dfcat20, sum )

