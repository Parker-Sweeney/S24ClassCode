install.package("jsonlite")
library(jsonlite)

nyt <- stream_in(file("newyorktimes.json"))

nyt$price <- as.numeric(nyt$price)

### Practice with gsub and replacing values

# Non gsub way
nyt$price[nyt$price == 19.99] <- 20.00

# Gsub way
nyt$price <- gsub(20.00, 19.99, nyt$price)


####################################
# Read in Raw Data for Office Employees
office <- read.csv("office_employees.csv")

### Two approaches to remove dups
# Logical Vector and ! to negate
office <- office[!duplicated(office),]

# unique()
office <- unique(office)

### Chack for multiple rows with same employee name
# check Frequency Counts
table(office$Employee)


##################################
# Read csv file
travel <- read.csv("travel_times.csv")

# Check if any NAs
anyNA(travel)

# Replace - with NAs
travel$FuelEconomy <- gsub("-", NA, travel$FuelEconomy)

# Count total NAs
sum(is.na(travel))

# Use colSums() to count by column
colSums(is.na(travel))
