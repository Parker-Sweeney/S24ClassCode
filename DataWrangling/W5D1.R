#Class 12 Example File (working with trave_times.csv and desmoines_housing.csv)
#Working with missing data
travel <- read.csv("travel_times.csv")
str(travel)
travel$FuelEconomy <- as.numeric(travel$FuelEconomy)
#Let's try some replacement techniques with gsub and conditional replacement:
travel$FuelEconomy[travel$FuelEconomy == ""] <- NA
travel$Comments[travel$Comments == ""] <- NA
travel$FuelEconomy <- gsub("-",NA,travel$FuelEconomy)

#Use colSums and is.na to count up NAs in each column:
colSums(is.na(travel))

#Let's figure out when they're missing:
travel_missing<-subset(travel, is.na(FuelEconomy))

#It looks like week chunks, some weeks driver forgot
#This is example of MCAR - missing completely at random
#because it's not dependent on the variable or it missing
#is not because of other variables - we can delete


#######Imputation#######
travel_edited<-na.omit(travel$FuelEconomy)

#Omits all rows with missing data

# missing values in ANY column
travel_edited<-na.omit(travel) # Returns data frame
# with only 23 rows :(

# Delete only rows with missing FuelEconomy
travel_edited2<-travel[!is.na(travel$FuelEconomy), ]

#Method 1: Fill NA with a constant
travel$FuelEconomy2<-travel$FuelEconomy
travel$FuelEconomy2[is.na(travel$FuelEconomy2)] <- 8

# Min and Max will not change as long as constant
# is within their range
min(travel$FuelEconomy, na.rm=TRUE)
min(travel$FuelEconomy2)

# Mean and Std. Deviation may change
mean(travel$FuelEconomy, na.rm=TRUE)
mean(travel$FuelEconomy2)
sd(travel$FuelEconomy, na.rm=TRUE)
sd(travel$FuelEconomy2)

# Method 2: Fill missing values based on summary statistic
travel$FuelEconomy3<-travel$FuelEconomy # Create copy
travel$FuelEconomy3[is.na(travel$FuelEconomy3)] <-
  mean(travel$FuelEconomy, na.rm=TRUE )

mean(travel$FuelEconomy, na.rm=TRUE)
mean(travel$FuelEconomy3)
sd(travel$FuelEconomy, na.rm=TRUE)
sd(travel$FuelEconomy3)

#Method 3:
# Method 3: Fill missing values based on "similarity"
# Example: average fuel economy based on speed
travel$FuelEconomy4<-travel$FuelEconomy # Create copy
# Divide observations into "Low" vs "High" speeds
#Use ifelse function
?ifelse
travel$Speed <- ifelse(travel$AvgSpeed <= median(travel$AvgSpeed),
                       "Low","High")
# Fill missing values with group means

# Fill NAs for "Low" speed
travel$FuelEconomy4[is.na(travel$FuelEconomy4) & travel$Speed == "Low"] <-
  mean(travel$FuelEconomy[travel$Speed =="Low"], na.rm=TRUE)

travel$FuelEconomy4[is.na(travel$FuelEconomy4) & travel$Speed == "High"] <-
  mean(travel$FuelEconomy[travel$Speed =="High"], na.rm=TRUE)

# Fill NAs for "High" speed

#######Data Reduction#######
## Read Raw Data ##
dsm <- read.csv("desmoines_housing.csv",
                na.strings = c("NA", "")) # Custom list of NAs
#Get dimensions of the dataset
dim(dsm)
# Calculate memory usage
?object.size # Returns object size in bytes
object.size(dsm)
format(object.size(dsm), units="MB")

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
dim(dsm)
## Reduce Rows ##
# Remove rows with illogical values
# Example: Property cannot have negative acres


# Combine multiple conditions with & (AND) or | (OR)


# Use ! to negate logical condition (TRUE>>FALSE)
# Example: Property cannot have NA address



# Remove rows that are irrelevant to proposed analysis, 
# but no "cherry-picking"
# Assume we will focus only on houses (not lots)

