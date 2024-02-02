# Open Local Radio CSV
abc <- read.csv("abc_local_radio.csv", na.strings=c(""," ","NA"))
abc2 <- read.csv("abc_local_radio.csv")

is.na()
!is.na()

# Import JSONLite
library(jsonlite)

# Parse JSON with stream_in()
nyt <- stream_in(file("newyorktimes.json"))

toronto <- stream_in(file("toronto_fashion.json"))
toronto2 <- stream_in(file("toronto_fashion.json"), flatten=TRUE)
  
# 
closedMon <- toronto2[is.na(toronto2$hours.Monday),]
