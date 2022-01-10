pkg <- c("tidyverse", "magrittr", "lubridate","RODBC", "DBI", "odbc", "colorspace", "plyr")
sapply(pkg, library, character.only = TRUE)

### SET OPTIONS/ENV VARIABLES ####

### Eliminate Scientific Notation
options(scipen = 999)

### SOURCE DATA/FUNCTIONS/FILES ####
filename.db <- paste0("C:/CreelDatabase/2017 Creel survey database all years1.mdb")
con <- dbConnect(odbc::odbc(),
                 .connection_string = paste("driver={Microsoft Access Driver (*.mdb)}",
                                            paste0("DBQ=", filename.db), "Uid=Admin;Pwd=;", sep = ";"),
                 timezone = "America/New_York")

dbListTables(con)
#Fish Caught Query from db
survey <- dbReadTable(con, "Survey Table")
#survey$Target.Species <- forcats::fct_explicit_na(survey$Target.Species)
unique(survey$Survey.Number)


df <- survey #%>% 
  group_by(Target.Species) %>% 
  count(Survey.Number)

  
df <- count(df, 'Target.Species')
df$prop <- df$freq/sum(df$freq)  
df1 <- df[c(8,11,12,17),]


