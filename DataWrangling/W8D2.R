rm(list=ls())
### Horizontal Data Integration ###

## Horizontal Merge ##
load("airline_tweets.rda")
str(all_tweets)
# Data frame with 14640 rows and 4 columns
# (user_name, tweet, airline, tweet_created)

airline_delays<-read.csv("airline_delays.csv")
str(airline_delays)
# Data frame with 6 rows and 8 columns (Airline, OnTime,
# CarrierDelay, WeatherDelay, LateAircraftDelay) 
# Tracking average proportion of delays for February 2015

?merge


# Specify join condition when column names do not match:
merged_data<-merge(all_tweets, airline_delays, 
                   by.x="airline", by.y="Airline")
# Could also change the column names to match before merging

# merge() defaults to an INNER JOIN
nrow(all_tweets) # 14640 tweets
nrow(merged_data) # Only 11727 in the merged data

# Add "all" argument to create an OUTER JOIN
# Keep all rows from all_tweets (LEFT OUTER JOIN)
merged_data_left<-merge(all_tweets, airline_delays, 
                        by.x="airline", by.y="Airline", 
                        all.x=TRUE)
nrow(merged_data_left)
View(merged_data_left)

# Keep all rows from airline_delays (RIGHT OUTER JOIN)
merged_data_right<-merge(all_tweets, airline_delays, 
                         by.x="airline", by.y="Airline", 
                         all.y=TRUE)
nrow(merged_data_right) 
View(merged_data_right)

# Keep all rows from both all_tweets and airline_delays 
# (FULL OUTER JOIN)
merged_data_all<-merge(all_tweets, airline_delays, by.x="airline", 
                       by.y="Airline",  all=TRUE)
nrow(merged_data_all) 



#QUANDL (Now owned by Nasdaq)
## Quandl ##
install.packages("Quandl")
library(Quandl)
# Message that methods are "masked" or overwritten
# just means that Quandl contains functions with the
# same names as base R or other packages

install.packages("NasdaqDataLink")
library(NasdaqDataLink)

#Can look at Zillow Home Prices
macro_data <- NasdaqDataLink.datatable("QDL/ODA", api_key = " ")
zillow_data <- Quandl.datatable("ZILLOW/DATA", api_key = " ")
?NasdaqDataLink.datatable

# Basic API call
?Quandl # Retrieve data from Quandl
platinum<-Quandl("LPPM/PLAT")
# LPPM/PLAT is "Quandl Code" (dataset ID) for fixing price of
# platinum (https://www.quandl.com/data/LPPM/PLAT-Platinum-Fixing)
class(platinum) # Result is returned as data frame
str(platinum)
# 7717 rows tracking opening (AM) and closing (PM) 
# price of platinum by day in US dollars, British 
# pounds, and Euros

# Change frequency with collapse argument
platinum_weekly<-Quandl("LPPM/PLAT",
                        collapse="weekly")
str(platinum_weekly)
# 1594 rows tracking opening and closing price at
# start of each week (Sunday)

# Change date range with start_date and end_date
# Example: Return only prices for 2019
platinum2019<-Quandl("LPPM/PLAT",
                     start_date="2019-01-01",
                     end_date="2019-12-31")





## Package-less APIs ##


library(httr)
library(jsonlite)

url <- "http://dataservice.accuweather.com/currentconditions/v1/328802?apikey=EquAxQJfNsvNA5IryJGjdbWOaeCAREje"
raw<-GET(url)

class(raw)
raw$status_code # 200 = okay

raw$content


# Parse JSON from API response
temperature<-fromJSON(content(raw, "text"), flatten=TRUE)

View(temperature)


#https://pokeapi.co/api/v2/pokemon

# install.packages("httr") 
library(httr)
library(jsonlite)

# Basic API Call
?GET # Get data from URL
url<-"https://pokeapi.co/api/v2/pokemon/pikachu"
raw<-GET(url)
class(raw) # Object of class "response"

# Check status of response
raw$status_code # 200 = okay

# Response content is encoded
raw$content

# View plain text of API response 
?content # Extract content from request
content(raw, "text")

# Parse JSON from API response
pikachu<-fromJSON(content(raw, "text"), flatten=TRUE)
# Note: Use fromJSON() when multiple JSON objects are 
# enclosed in comma-separated list

## Lists ##
class(pikachu) # Result is a list
length(pikachu) # 17 elements (with nested sub-elements)

# Access list elements with [[]]
pikachu[[1]] # Data frame with 4 attributes
# for 2 abilities
pikachu[["abilities"]] # Can use named index
pikachu$abilities # Or $ notation

# Access sub-elements from data frame
pikachu$abilities[ ,"ability.name"]
pikachu$abilities$ability.name

# Some elements of the list have just one value
pikachu$name
