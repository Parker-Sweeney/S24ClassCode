### Tidyverse ###

# install.packages("tidyverse")
library(tidyverse)

# install.packages("janitor")
library(janitor)


###Import data
#readr
ufo <- read.csv("ufo_tidy.csv")
str(ufo) #data is read in as a tibble (https://r4ds.had.co.nz/tibbles.html)

ufo2<-read.csv("ufo_tidy.csv", na.strings=c("-",""," ","NA","N/A"), encoding="UTF-8")
str(ufo2)

print(ufo)
print(ufo2)


ufo <-ufo2


###Clean column names
#janitor
ufo <- ufo %>% clean_names()

# Reformat Column names
ufo <- clean_names(ufo, "snake")
ufo3 <- clean_names(ufo, "upper_camel")


#Rename columns
#tidyr
ufo2 <- rename(ufo2, witnesses = number_of_witnesses) 


###Date
#lubridate
str(ufo)
library(lubridate)

ufo2$sighting_date_edited <- mdy(ufo2$SightingDate)

ufo2$posted_date_edited <- dmy(ufo2$PostedDate)
str(ufo2)


#Remove duplicate rows in a data frame
#dplyr


distinct(ufo2)

ufo2 <- distinct(ufo2)

ufo <- ufo2

sort(table(ufo2$city), decreasing=TRUE)


ufo2$duration_edited <- ufo2$Duration
#ufo2$duration_edited <- as.factor(ufo2$duration_edited)
#levels(ufo$duration_edited)

#Mutate (replaces data with different value)
#dplyr
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "1-2 minutes", "1.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "2-3 minutes", "2.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "3-4 minutes", "3.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "5-7 minutes", "6 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "7-8 minutes", "7.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "8-10 minutes", "9 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "7 & 6 minutes", "6.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "8-10 minute", "9 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "2-3 seconds", "2.5 seconds"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "13-20 seconds", "16.5 seconds"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "20-30 seconds", "25 seconds"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "3-5 minutes", "4 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "5:00", "5 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "15-20 minutes", "17.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "45-60 seconds", "52.5 seconds"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "10-15 minutes", "17.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "12:30 a.m. 3:30 a.m.", "3 hours"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "0:00:17 seconds", "17 seconds"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "3 minutes each", "3 minutes"))
ufo2$duration_edited[(ufo2$duration_edited =="A few seconds")]<-NA
ufo2$duration_edited[(ufo2$duration_edited =="until sunrise")]<-NA
ufo2$duration_edited[(ufo2$duration_edited =="Ongoing")]<-NA
ufo2$duration_edited[(ufo2$duration_edited =="0.05")]<-NA
ufo2$duration_edited[(ufo2$duration_edited =="1")]<-NA
ufo2$duration_edited[(ufo2$duration_edited =="UNKNOWN")]<-NA
ufo2$duration_edited[(ufo2$duration_edited =="Unknown")]<-NA
ufo2$duration_edited[(ufo2$duration_edited =="seconds")]<-NA
ufo2$duration_edited[(ufo2$duration_edited =="I don't know")]<-NA
ufo2$duration_edited[(ufo2$duration_edited =="30- NA")]<-NA
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "~1 hour", "1 hour"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "~2 hours", "2 hours"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "~3 minutes", "3 minutes"))
ufo2 <- ufo2 %>% mutate(duration_edited = replace(duration_edited, duration_edited == "~5 seconds", "5 seconds"))


unique(ufo2$duration)
unique(ufo2$duration_edited)

ufo <- ufo2

#Regular Expressions
# https://roblocher.com/technotes/regexp.html
# ^[^ ]
# ^[ ]
# [^ ]+$

ufo2 <- ufo

#Split a column into 2 columns
#dplyr

ufo2 <- separate(ufo2, duration_edited, into = c("duration2", "time_frame"), sep = " (?=[^ ]+$)")

str(ufo2)




#Mutate multiplier to get duration in seconds
#dplyr
ufo2 <- mutate(ufo2, time_frame = ifelse(time_frame == "minutes" | time_frame == "minute", 60, time_frame))
ufo2 <- mutate(ufo2, time_frame = ifelse(time_frame == "hours" | time_frame == "hour", 3600, time_frame))
ufo2 <- mutate(ufo2, time_frame = ifelse(time_frame == "seconds" | time_frame == "second", 1, time_frame))

# If positive duration, take duration2 by new time frame (to get num of secs)
ufo2 <- mutate(ufo2, duration_seconds = ifelse(duration2 > 0, as.numeric(duration2) * as.numeric(time_frame), duration2))

#Remove a column
#dplyr
ufo2 <- select(ufo2, -duration2)
ufo2 <- select(ufo2, -time_frame)
ufo2 <- select(ufo2, -posted_date)
ufo2 <- select(ufo2, -duration)


#Reorder columns
#dplyr

ufo2 <- select(ufo2, sighting_date_edited, city, state, duration_seconds, shape, intensity, summary, witnesses, posted_date_edited)
