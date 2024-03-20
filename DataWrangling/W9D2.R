#Class 24 Blank

rm(list=ls())

## RSocrata ##
install.packages("RSocrata")
library(RSocrata)

# Basic API call
?read.socrata # Return Socrata dataset
# Create query string/URL that starts with unique
# "endpoint" of dataset
url<-"https://data.iowa.gov/resource/3adi-mht4.json"
expenditures<-read.socrata(url) # Large dataset
# Will take a few minutes to run 
class(expenditures) # Result is returned as data frame
str(expenditures)
# 565131 rows tracking campaign expenditures by committes 
# and PACs
# Records ID, date, committee making the expenditure,
# person/organization paid, and amount

# Optional arguments include app_token and email (for 
# users with Socrata developer accounts)

#SoQL Query where I only want February 2024 expenditures (use AND)
#With a date, you need to have it in ' '  single quotes

## SoQL Queries ##
# SQL-style queries start with ? after dataset endpoint
# Add $where clause to filter data
# Example: Return expenditures by Iowa Democratic Party
url<-"https://data.iowa.gov/resource/3adi-mht4.json?$where=committee_nm='Iowa Democratic Party'"
# NOTE: Avoid line breaks in query strings/URLs
expenditures_IDP<-read.socrata(url) 
str(expenditures_IDP)
# 23271 rows for expenditures by IDP

# Add $select clause to return specific columns
# Example: Return only date and amount for IDP expenditures
url<-"https://data.iowa.gov/resource/3adi-mht4.json?$select=date,amount&$where=committee_nm='Iowa Democratic Party'"
# NOTE: Separate different SoQL clauses with &
expenditures_IDP2<-read.socrata(url) 
str(expenditures_IDP2)
# 20180 rows, and only 2 columns

# Add $group clause to aggregate data and $order clause to sort
# Example: Return total expenditures for each committee
# and sort results in alphabetical order by committee name
url<-"https://data.iowa.gov/resource/3adi-mht4.json?$select=committee_nm,SUM(amount)&$group=committee_nm&$order=committee_nm ASC"
expenditures_committee<-read.socrata(url) 
str(expenditures_committee)

# Check out SODA Queries docs for templates 
# https://dev.socrata.com/docs/queries/











### Descriptive Statistics ###

## Read and Examine Data ##
mortgage<-read.csv("mortgage.txt",
                   sep="|", 
                   na.strings=c("NA","-",""),
                   colClasses=c("integer", 
                                "integer","integer", 
                                "integer","integer", 
                                "factor"))
# Tracks customer features for home mortages, 
# include defaults and non-defaults
str(mortgage) # 1295 rows and 6 columns
colSums(is.na(mortgage)) # 3 columns have missing
# values (YearsEmployed, CreditCardDebt, Year)

## Statistics for One Continuous Variable ##
# Central tendency describes the center point of 
# the data based on a specific metric
# Arithmetic mean = sum of all values / n
mean(mortgage$CreditScore) 

# Median = 50th percentile 
median(mortgage$CreditScore) 

# Spread describes the extremes and variation 
# within data values
# Min and Max = extrema
min(mortgage$CreditScore) 
max(mortgage$CreditScore) 

# Range = max - min
max(mortgage$CreditScore)-min(mortgage$CreditScore) 

# Interquartile range = 75th percentile-25th 
IQR(mortgage$CreditScore)

# Variance = average of squared difference between 
# each data point and the mean
var(mortgage$CreditScore) 

# Standard deviation = square root of variance
sd(mortgage$CreditScore) 

# summary() function provides several stats at 
# once: min, max, mean, median, quantiles, NAs
summary(mortgage$CreditScore)

# Interpreting statistics:
# Credit scores range from 537 to 840
# Average score is 696.7, but median is 696
# (suggests distribution is unlikely to be skewed)
# SD is 49.6 (credit score is 49.6 points
# away from the mean on average)

# Shape can be confirmed with visualization
?boxplot # Box and whisker plot
boxplot(mortgage$CreditScore)
# Shape is symmetrical (not skewed), 
# with few outliers

?hist # Histogram 
hist(mortgage$CreditScore,      
     main="Credit Score") # Add main title
# Shape is symmetrical, and unimodal (one peak)
# Somewhat bell-shaped (could be normally 
# distributed)

# Check normality with Q-Q plot
?qqnorm # Quantile-Quantile plot compares actual
# quantiles to expected with normal distribution 
qqnorm(mortgage$CreditScore) 
# Add reference line for perfect correlation
qqline(mortgage$CreditScore, col="red") 
# Interpretation: marks stay close to the 
# red line, suggesting that Credit Score 
# is very close to a normal distribution

# Hypothesis test for normal distribution
?shapiro.test # H0 = data is normally distributed
shapiro.test(mortgage$CreditScore)
# Interpretation: p-value > 0.05, thus we cannot 
# reject the null hypothesis that the data is 
# normally distributed

## Statistics for One Categorical Variable ##
# Measures of frequency include counts
#0 - did not default on loan; 1 - defaulted on loan
table(mortgage$Default)

# Mode = most frequent value
names(sort(table(mortgage$Default), 
           decreasing=TRUE))[1]

# Calculate proportions based on frequency table
?prop.table 
prop.table(table(mortgage$Default)) 

# Investigate frequency with visualizations
?barplot # Bar/column chart
# Input is frequency table or vectors storing
# bar lengths and labels
barplot(table(mortgage$Default)) 

barplot(table(mortgage$Default),       
        main="Mortage Defaults", # Title
        xlab="Frequency", # X axis label
        xlim=c(0,1000), # X axis range
        horiz=TRUE) # horiz=TRUE for bar chart

?pie # Pie Chart
# Input is frequency table
pie(table(mortgage$Default), 
    col=c("gray","red"),
    labels=c("No Default", "Default")) # Labels

# Note: Pie charts should not be used in 
# situations with many categories 
# (rule of thumb: <= 5) 
