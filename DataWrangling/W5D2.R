
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
# Not useful: book, pg (irrelevant)


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

dsm_keep <- dsm[,cols]
#Use colSums and is.na to count up NAs in each column:
colSums(is.na(dsm))

#Remove columns with - sign and select, subset
new_dsm <- subset(dsm, select= -c(book,pg,veneer_area))

## Reduce Rows ##
# Remove rows with illogical values
# Example: Property cannot have negative acres or negative price
dsm<-subset(dsm, total_living_area>=0)
dsm<-subset(dsm, price>=0)
dsm<-subset(dsm, land_acres>=0)

# Combine multiple conditions with & (AND) or | (OR)
dsm<- subset(dsm,rooms>=0 & bathrooms>=0)

# Use ! to negate logical condition (TRUE>>FALSE)
# Example: Property cannot have NA address
dsm <- subset(dsm, !is.na(dsm$address))

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

# Min/max normalization always produces positive numbers 
# with the same 0-1 range
# Different mean, deviation, distribution

########Factors########
# Levels are limited set of possible values that
# can be stored in factor
#Store city name as levels


## Discretization ##
# AC is currently on a scale from 0-100 (percentage of 
# house covered by AC). Convert AC to a 0/1 factor based 
# on whether 50% or more is covered 
?ifelse # Apply simple conditional statements to vectors
dsm$air <- dsm$air_conditioning

dsm$air <- ifelse(dsm$air>=50,1,0)
dsm$air <- as.factor(dsm$air)
# Transform result to factor
dsm$air_conditioning<-as.factor(dsm$air_conditioning)

#Discretizing is good for machine learning
