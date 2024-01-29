dmh <- desmoines_housing
transform(dmh$price <- as.numeric(dmh$price))
#Sort with Dataframe using order()
dmh_sorted <- dmh[order(dmh$price, decreasing = TRUE),]
dmh_sorted
#Dataframe using which - remember [rows,column]
dmh_flip <- dmh[which(dmh$price < 2001),]
dmh_flip
#####################
getwd()
#setwd(...) if need to change working directory with code
save(dmh_flip, file = "low-cost-housing.rda")
################################

# Read csv file and set it to variable
allstate <- read.csv("allstate.txt")
allstate

# Set dataframe to new variable name
allstate2 <- allstate
str(allstate2)

# All costs less than $600
allstate3 <- allstate2[which(allstate2$cost < 600),]
allstate3
# Create New CSV file with results of $600 query
write.csv(allstate3, "low-cost-insurance.csv")
