rm(list=ls())
### Data Cleaning ###

## Raw Data ##
office<-read.csv("office_employees.csv") 
str(office)
View(office)

## Duplicate Data ##
?duplicated # Returns logical vector with TRUE for elements
# (rows) that are an exact duplicate of a previous element
duplicated(office)
sum(duplicated(office)) # 2 duplicate records 

# View duplicates
office[duplicated(office),]
# Only the second copy of each record (the duplicate) 
# returns TRUE

# Two approaches to remove duplicates:
# Use logical vector and ! to negate
office<-office[!duplicated(office), ]

# Or use unique() function:
?unique # Extract unique elements/rows
office<-unique(office)

# What about near duplicates?
# Check for multiple rows with same employee name:
?table # Calculate frequency counts
table(office$Employee)

# Sort frequency table for easier reading:
sort(table(office$Employee), decreasing=TRUE)
# Two employee records for Meredith Palmer

# Can also use duplicated() to find repeating names
office$Employee[duplicated(office$Employee)]

# View records with duplicated name
office[office$Employee==office$Employee[duplicated(office$Employee)],]
# Records are exactly the same except for age

# Remove second copy for Meredith (age 48)
office<-office[-duplicated(office$Employee),]


office <- office[-which(office$Employee=="Meredith Palmer" & office$Age==48),]


office_org <- office
office <- office_org


## Data Type Errors ##
str(office)
# Age should be number
# Salary should be number
# HireDate should be date

# Transform Age
office$Age_Edited<-office$Age # Create copy

office$Age_Edited<-as.numeric(office$Age_Edited) 
# Warning: Some ages could not be converted into numbers

# How many records have NAs in the Age_Edited column?
sum(is.na(office$Age_Edited)) 

# Which row(s) produced the error
office[is.na(office$Age_Edited),]
# Kelly's age is listed in words 

# Fix the NA value with the correct age (28)
office$Age_Edited[is.na(office$Age_Edited) & office$Employee=="Kelly Kapoor"]<-28

# Are all Age_Edited values numeric now?
is.numeric(office$Age_Edited)


office_org <- office
office <- office_org



# Transform Salary
office$Salary_Edited<-office$Salary # Create copy
office$Salary_Edited<-as.numeric(office$Salary_Edited)
# Warning: Some salaries could not be converted to numbers

office[is.na(office$Salary_Edited),]

# Examine Salary closely. What is the issue?
office$Salary[1:5]
# Salaries have commas and text "per year"

# Use text functions to clean data before transforming
office$Salary_Edited<-office$Salary # Create copy
# Find all commas and replace with nothing
office$Salary_Edited<-gsub(",","",office$Salary_Edited)
office$Salary_Edited[1:5]

# Can use gsub() or strsplit() to remove "per year"
?strsplit # Divide text based on delimiter/pattern 
office$Salary_Edited<-strsplit(office$Salary_Edited, " ")
class(office$Salary_Edited) # Result is a list
office$Salary_Edited # Each list element has 3 sub-elements:
# 1=salary, 2=per, 3=year

# Extract only first sub-element from each element
?sapply # Apply function to list
office$Salary_Edited<-sapply(office$Salary_Edited, 
                             "[[", 1)
office$Salary_Edited[1:5]

#alternative
office$Salary_Edited<-gsub(" per year","",office$Salary_Edited)


office$Salary_Edited3 <- NULL

# Transform to number
office$Salary_Edited<-as.numeric(office$Salary_Edited)

office$Salary_Edited <- as.numeric(office$Salary_Edited)


max(office$Salary_Edited)

office$Salary_Edited <- office[which(office$Salary_Edited<30000),"Salary_Edited"]


# Check for salaries that are too low or too high:
rows<-which(office$Salary_Edited < 10000
            | office$Salary_Edited > 1000000)
rows

# Examine erroneous data
office[rows, ]
# Phyllis' salary listed as 51
# Jim's salary listed as 44

# Transform to thousands:
office$Salary_Edited[rows]<-office$Salary_Edited[rows]*1000













#HERE MWC

office$HireDate

# Transform HireDate
office$HireDate_Edited<-office$HireDate # Create copy
office$HireDate_Edited<-as.Date(office$HireDate_Edited) 
# This line will throw an error

# Examine HireDate more closely. What is the issue?
office$HireDate_Edited
# Two different date formats: %d-%b-%y and %B %d, %Y
# Neither are the R preferred format: %Y-%m-%d

# Convert all dates in the first format:
office$HireDate_Edited<-as.Date(office$HireDate, 
                                "%d-%b-%y")



is.na(office$HireDate_Edited)


# Find and convert the NA values (dates in alternate format)
office$HireDate_Edited[is.na(office$HireDate_Edited)]<-
  as.Date(office$HireDate[is.na(office$HireDate_Edited)], 
          "%B %d, %Y")
class(office$HireDate_Edited)





## Data Format Errors ##
str(office)
# Commission is coded as Y/N, should be logical

?ifelse # Apply simple conditional logic to elements 
# in a vector
office$Commission_Edited<-ifelse(office$Commission=="Y", # Test
                                 TRUE, # Value if true
                                 FALSE) # Value if false
is.logical(office$Commission_Edited)
sum(is.na(office$Commission_Edited))

## Illogical Data ##
# Write logical tests (e.g., find employees with ages
# less than 10 or greater than 100)
which(office$Age_Edited < 10 | office$Age_Edited > 100)
# integer(0) means no results were returned

# Check for hiring dates that are too far in the 
# past or future:
rows<-which(office$HireDate_Edited < Sys.Date()-36500 
            | office$HireDate_Edited > Sys.Date())
rows # Row 12 has an illogical hire date

# Examine erroneous data
office[rows, ]
# Toby's hire date is listed as 2025

# Assume we know Toby's hire date is actually 2005:
office$HireDate_Edited[rows]<-as.Date("2005-03-06")

# NOTE: If Toby's true hire date is unknown,
# would be better replace with NA

# Check for salaries that are too low or too high:
rows<-which(office$Salary_Edited < 10000
            | office$Salary_Edited > 1000000)
rows

# Examine erroneous data
office[rows, ]
# Phyllis' salary listed as 51
# Jim's salary listed as 44

# Transform to thousands:
office$Salary_Edited[rows]<-office$Salary_Edited[rows]*1000

## Typos and Misspellings ##
# Apply unique() function to column to view vector of 
# all possible values:
unique(office$Title_Edited)

# Sorting makes it easier to read/compare:
sort(unique(office$Title_Edited),decreasing=FALSE)
# "Accountnt" is typo for "Accountant"

# Create a copy of Title column called Title_Edited then 
# find and fix rows with typos:
office$Title_Edited<-office$Title
office$Title_Edited[which(office$Title_Edited=="Accountnt")]<-"Accountant"

# Can also use the table() function for spotting 
# potential errors:
sort(table(office$Gender), decreasing=TRUE)

# Assume we want to code gender as "Male" or "Female"
office$Gender_Edited<-office$Gender # Create copy
office$Gender_Edited[which(office$Gender_Edited=="M")]<-"Male"
office$Gender_Edited[which(office$Gender_Edited=="F")]<- "Female"
unique(office$Gender_Edited)

office$Gender <- NULL
office$Age <- NULL
office$Salary <- NULL




# Data file is nice and clean!
# Save file as CSV or R data file
write.csv(office,"office_clean.csv",row.names=FALSE)
save(office,file="office_clean.rdata")
