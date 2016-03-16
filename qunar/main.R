library(dplyr)
library(plyr)
library(stringr)

# setwd("/Users/shihchosen/Documents/Github/AirplaneTicketScrapy/qunar/")
source("./getFlightInfos.R")

files <- list.files(path = "./data/3_14/")%>%
  str_c("./data/3_14/",.)
files
data314 <- lapply(files, getFlightInfos, queryDate ="2015-03-14")%>%
  do.call("rbind",.)
write.csv(data314, "./data/output/data314.csv", row.names = FALSE)



  
