library(lubridate)
### Opening Day function - WIP  
# openingday <- function(y){
#   year <- paste(as.character(year(Sys.Date())))
#   start <- paste(year, "01", "01", sep = "-")
#   end <- paste(year, "12", "31", sep = "-")
#   x <- seq(ymd(start), ymd(end), by="1 day")
#   saturdays <- x[wday(x,label = TRUE) == "Sat" & day(x) <=7]
#   openingday <- saturdays[4]
# 
# } 
# openingday()
# 

### create opening day variable 
year <- paste(as.character(year(Sys.Date())))
### OR set year 
# year <- 2017 
start <- paste(year, "01", "01", sep = "-")
end <- paste(year, "12", "31", sep = "-")
x <- seq(ymd(start), ymd(end), by="1 day")
saturdays <- x[wday(x,label = TRUE) == "Sat" & day(x) <=7]
openingday <- saturdays[4]
openingday
