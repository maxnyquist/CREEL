library(readxl)
library(tidyr)
library(xlsx)
getwd()
#setwd as the location of all raw files and the desired destination for export from R files. 
setwd("W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Creel Survey related/2017 Creel Survey")
#create variable of the RAW Survey Table from Creel survey database. Filepath may be different. 
Survey_TableRAW <- read_excel("W:/WatershedJAH/EQStaff/Aquatic Biology/Fish/Creel Survey related/2017 Creel Survey/Survey Table2.0.xlsx")

#create new variable to get rid of unnecessary columns. 
# All that should remain is Loop Number, Survey Number, Survey Date, and all five columns of Zip Codes (HomeZipCode, Zip2-5).
zipinfo <- Survey_TableRAW[, c(1:3, 12:16)]

#create variable to move all zip code columns into new column "ZipCode". 
# "ZipNum" is the key, which represents the original column the zip code came from (HomeZipCode, Zip2-5). 
#if no zip code value from Survey_TableRAW, not gathered. 
?gather
zipinfo <- gather(zipinfo, "ZipNum", "ZipCode", 4:8, na.rm =T)
#gather(our data frame Zipinfo, "ZipNum" is the new column and the key and represents the original column
# "ZipCode is the new column containing all zip codes, ... (4:8) is the selection of columns to gather
# na.rm should be T or TRUE to remove rows from the output where the value column is NA

str(Longformatdata)
#write and export cleaned files for import into Access. These will be the AllSurveyZipCodesStacked Tables
write.csv(Longformatdata, "ZipCodeClean.csv")
write.xlsx(Longformatdata, "ZipCodeClean.xlsx")



