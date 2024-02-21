#######Data Reduction#######
## Read Raw Data ##
dsm <- read.csv("desmoines_housing.csv",
                na.strings = c("NA", "")) # Custom list of NAs
dim(dsm) # 17513 rows, 60 columns

# Calculate memory usage
?object.size # Returns object size in bytes
object.size(dsm)
format(object.size(dsm), units = "Mb") # Format size as MB

#Let's look at first 10 columns
sub1<-dsm[1:10,1:10] # Examine snippet of first 10 columns
View(sub1)
# Sort into useful, potentially useful, or not useful
# Useful: jurisdiction, nbhd, school_district, date, price
# Potentially useful: instrument, address, zip
# Not useful: book, page (irrelevant)


# Create a vector with names of columns to keep
cols <- c("jurisdiction","nbhd","school_district","sale_date",
          "price","address","zip","quality1","quality2",
          "land_acres","occupancy","residence_type",
          "bldg_style","exterior_wall_type","roof_type",
          "roof_material","total_living_area","foundation",
          "basement_area","fin_bsmt_area_tot","bsmt_walkout",
          "att_garage_area","open_porch_area",
          "enclose_porch_area","patio_area","deck_area",
          "carport_area","bathrooms","toilet_rooms",
          "whirlpools","hottubs","saunas","fireplaces",
          "bedrooms","rooms","year_built","year_remodel",
          "condition","grade","heating","air_conditioning")
dsm <- dsm[,cols]

#Use colSums and is.na to count up NAs in each column:
colSums(is.na(dsm))

#Remove columns with - sign and select
new_dsm <- subset(dsm, select=-c(book,pg))

## Reduce Rows ##
# Remove rows with illogical values
# Example: Property cannot have negative acres or negative price
dsm<-subset(dsm, land_acres>=0) 
dsm<-subset(dsm, total_living_area>=0) 
dsm<-subset(dsm, price>=0) 

# Combine multiple conditions with & (AND) or | (OR)
dsm<-subset(dsm, rooms>=0 & bathrooms>=0) 
dsm<-subset(dsm, fin_bsmt_area_tot>=0 & fireplaces>=0) 

# Use ! to negate logical condition (TRUE>>FALSE)
# Example: Property cannot have NA address
dsm<-subset(dsm, !is.na(dsm$address))
dsm<-subset(dsm, !is.na(dsm$zip))

# Remove rows that are irrelevant to proposed analysis, 
# but no "cherry-picking"
# Assume we will focus only on houses (not lots)
dsm<-subset(dsm, quality1!="Vacant Lot" & 
              quality2!="Vacant Lot")

#Feature scaling (normalization)
# Min/Max normalization places values in a 0-1 scale
# Formula: (x - min) / (max - min)
dsm$acres <- dsm$land_acres
acres_max <- max(dsm$acres)
acres_min <- min(dsm$acres)
dsm$acres_norm <- (dsm$acres - acres_min)/ (acres_max - acres_min)


# Z-score standardization rescales values so that the mean 
# is 0 and standard deviation is 1
# Formula: (x - mean) / standard deviation
acres_mean<- mean(dsm$acres)
acres_sd<- sd(dsm$acres)
dsm$acres_standardized <- (dsm$acres - acres_mean) / acres_sd

summary(dsm$acres_standardized) 
summary(dsm$living_area_standardized)
# Z-score standardization can produce negative numbers with 
# different ranges
# Same mean (0) and deviation (1)

## Discretization ##
# AC is currently on a scale from 0-100 (percentage of 
# house covered by AC). Convert AC to a 0/1 factor based 
# on whether 50% or more is covered 
?ifelse # Apply simple conditional statements to vectors
dsm$air_conditioning<-ifelse(dsm$air_conditioning>=50,1,0)
is.factor(dsm$air_conditioning) # Result is vector
dsm$air_conditioning <- as.factor(dsm$air_conditioning)
levels(dsm$air_conditioning)
# Levels are limited set of possible values that
# can be stored in factor
dsm[232,"jurisdiction"] # View one city name
unique(dsm$jurisdiction)
levels(dsm$jurisdiction) # 12 cities
#dsm$jurisdiction <- as.factor(dsm$jurisdiction)

## Reorder Levels ##
# Levels are sorted (alphabetical by default)
dsm$condition
dsm$condition <- as.factor(dsm$condition)
levels(dsm$condition)
dsm$condition <- factor(dsm$condition,
                        level=c("Very Poor","Poor",
                                "Below Normal","Normal",
                                "Above Normal", "Very Good",
                                "Excellent"))
level(dsm$condition)
# Use factor() with levels argument to specify order
dsm$condition<-   
  
level(dsm$condition) # Reordered

#Let's try with nba.txt





### Tidyverse ###

install.packages("tidyverse", dependencies=TRUE)
library(tidyverse)
install.packages("tzdb")
install.packages("ggplot2", dependencies=TRUE)

install.packages("janitor")
library(janitor)

ufo<-read.csv("ufo_tidy.csv", na.strings=c("-",""," ","NA","N/A"), encoding="UTF-8")
str(ufo)

#Clean column names
#janitor   https://rpubs.com/jenrichmond/clean_names
ufo <- ufo %>% clean_names()

ufo <- clean_names(ufo, "screaming_snake")
ufo <- clean_names(ufo, "upper_camel")

#Date
#lubridate
str(ufo)
library(lubridate)

ufo$SightingDate <- mdy(ufo$SightingDate)
ufo$PostedDate <- dmy(ufo$PostedDate)




library(dplyr)
duplicated(ufo2)
sum(duplicated(ufo2))
unique(ufo2)


#distinct function in dplyr

ufo2 <- ufo
ufo <- ufo2

ufo2$duration_edited <- ufo2$Duration
ufo2$duration_edited

#Mutate
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
unique(ufo2$duration_edited)#https://dplyr.tidyverse.org/#overview
