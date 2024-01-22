# Clear R Environment
rm(list-ls())
# Set variable to the given vector
vector1 <- c(1,3,5,7)
# Check if Vaiable is a Vector
is.vector(vector1)
# Check the Data Type of the Variable
typeof(vector1)
# Set New vector to a variable
vector2 <- c(1:5)
# Use to get help with the sequence seq() function
# ?seq
# Set given sequence to a variable
vector3 <- seq(1,10,2)
# Index the new variable
vector3[3:5]
# Use negative index with new variable
vector3[-3]
# Which values in variable are greater than 5
which(vector3>5)
############################################################
# Load Rivers Dataset
data(rivers)
# Find info on dataset
?rivers
# Get min and max in rivers dataset
min(rivers)
max(rivers)
