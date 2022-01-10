library(readxl)
library(tidyr)
Survey_TableRAW <- read_excel("W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Creel Survey related/Survey TableRAW.xlsx")
#get rid of unnecessary columns
Zipinfo <- Survey_TableRAW[, c(1:2, 11:15, 19)]
View(Zipinfo)
# Survey_Table <- Survey_TableRAW[,-3:9]
# 1: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
# Expecting logical in O1688 / R1688C15: got '01094'
# 2: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
# xpecting logical in O1881 / R1881C15: got '01420'
# 3: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
# Expecting logical in O1882 / R1882C15: got '01606'
# 4: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
# Expecting logical in O1929 / R1929C15: got '01970'
# 5: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
# Expecting logical in O1960 / R1960C15: got '01605'
??gather

#move all zip code columns into new column "ZipCode". "ZipNum" is the key, which represents the original column 
#the value came from. 
#if no zip code value from Survey_TableRAW, not gathered. 
Longformatdata <- gather(Zipinfo, "ZipNum", "ZipCode", 3:7, na.rm =T)

str(Longformatdata) #
#If there is a single survey, or two surveys on the same date, can switch the Zip with this
# Longformatdata$ZipCode[Longformatdata$ZipCode == "76312"] <- "76132"
# Longformatdata$ZipCode[Longformatdata$ZipCode == "01483"] <- "01453"
# Longformatdata$ZipCode[Longformatdata$ZipCode == "01412"] <- "01462"
# Longformatdata$ZipCode[Longformatdata$ZipCode == "01514"] <- "01510"
# Longformatdata$ZipCode[Longformatdata$ZipCode == "01483"] <- "01453"
# Longformatdata$ZipCode[Longformatdata$ZipCode == "01712" & Longformatdata$SurveyDate == 2017-11-12] 
#Longformatdata$ZipCode[Longformatdata$SurveyDate == 2011-06-04, Longformatdata$ZipCode == "01404"] <- "01464"
# Longformatdata$ZipCode[as.numeric(rownames(Longformatdata == "01712"))] 


#changed survey 3191 zip code from 01712 to 01772
# Longformatdata$ZipCode[Longformatdata$`Survey Number` == "3191" & Longformatdata$ZipCode == "01712" ] <- "01772"
# #Locate the entire record by Survey Number
# Longformatdata[Longformatdata$`Survey Number` == "2145",]
# #Look up the zip code within this record to confirm the change
# Longformatdata$ZipCode[Longformatdata$`Survey Number` == "2145"]
# #change the zip code
# Longformatdata$ZipCode[Longformatdata$`Survey Number` == "2145" & Longformatdata$ZipCode == "01716" ] <- "01776"
# Longformatdata[Longformatdata$`Survey Number`== "2830",]
# Longformatdata$ZipCode[Longformatdata$`Survey Number` == "2830"]
# Longformatdata$ZipCode[Longformatdata$`Survey Number` == "2830" & Longformatdata$ZipCode == "01404" ] <- "01904"
# Longformatdata[Longformatdata$`Survey Number`== "3192",]
# Longformatdata$ZipCode[Longformatdata$`Survey Number` == "3192"]
# Longformatdata$ZipCode[Longformatdata$`Survey Number` == "3192" & Longformatdata$ZipCode == "01479" ] <- "01749"
# ZipCodeClean <- Longformatdata


#This changed zip 01404 for Survey Number 2830 and survey 540, however this change was not maintained if rownames switched. 
# Longformatdata$ZipCode[as.numeric(rownames(Longformatdata)) == 423] <- "01464"  
# Longformatdata$ZipCode[as.numeric(rownames(Longformatdata)) == 2314] <- "01904"  
# Longformatdata$ZipCode[as.numeric(rownames(Longformatdata)) == 5207] <- "01749"
# Longformatdata$ZipCode[as.numeric(rownames(Longformatdata)) == 2488] 
# Longformatdata$ZipCode[(rownames(Longformatdata$ZipCode)) == "01712"] 

# Longformatdata$ZipCode[Longformatdata$ZipCode == "01712" & Longformatdata$SurveyDate == 2017-11-12,] 
getwd()
library(readxl)
library(xlsx)
write.csv(ZipCodeClean, "ZipCodeClean.csv")
write.xlsx(ZipCodeClean, "ZipCodeClean.xlsx")
#?attach
#?which



