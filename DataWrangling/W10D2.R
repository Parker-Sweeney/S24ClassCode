
## Read Data ##
students<-read.csv("students_math.csv",
                   stringsAsFactors=TRUE)
str(students) 

## Logistic Regression ##
# Create a new column called pass that
# stores 1 if final grade is greater than
# 10, 0 otherwise
students$pass <- ifelse(students$final>10,1,0) #Use ifelse


table(students$pass) # 186 failures, 209 passes

# Is the likelihood of passing math related 
# to the number of previous failures?
?glm # Fit generalized linear models based 
# on formula
fit<-glm(pass~failures, data=students, family=binomial(link="logit"))

summary(fit)
# Coefficients represent effect on log-odds
# Apply exponential function to represent
# change in odds
# Failures: 
exp(-1.2089) # Each additional failure is 
# associated with 70% decrease in odds of passing

# No R-squared, calculate pseudo R-squared:
1-(fit$deviance/fit$null.deviance)
# Very low. Model is not a good fit
# for the data


## Multiple Logistic Regression ##
# Is the likelihood of passing math related
# to weekly study time, academic support from 
# the school, whether the student has paid 
# tutoring, number of previous failures, 
# and number of absences?
fit2<-glm(pass~study_time+school_sup+
            paid+failures+absences, 
          data=students,
          family=binomial(link='logit'))
summary(fit2)
# School support:
exp(-0.922903) # Having no academic support
# from the school is associated with 60% decrease
# in odds of passing
# Failures:
exp(-1.210998) # Each additional failure is
# associated with 70% decrease in odds of passing

# Pseudo R-squared:
1-(fit2$deviance/fit2$null.deviance)


##################################################################

#                 End of Log Reg Part of Code                    #

##################################################################


# Load wine ratings data:
wine<-read.csv("wine_ratings.txt", sep="|")
# Data on 14,781 wine ratings including variety,
# winery, country, price, reviewer, and rating

# Load scales and ggplot2 packages:
library(scales)
library(ggplot2)

## Visualization with 2 Continuous Variables ##
# Scatter plot: visualize relationship between 
# 2 numeric variables
# "Base R" scatterplot (wine price and rating):
plot(wine$Points~wine$Price) # formula: Y ~ X 

# ggplot2 scatterplot
qplot(Price, # x (horizontal)
      Points, # y (vertical)
      data=wine, # data source
      geom="point") + # plot type
  scale_x_continuous(labels = dollar) # $ format

# Many overlapping data points, add colors to 
# show frequency (similar to heatmap)
qplot(Price,          
      Points,  
      data=wine, 
      geom="bin2d") + # plot type (2D bins)
  scale_x_continuous(labels = dollar)

# Can also use transparency for overlapping 
# points:
qplot(Price,          
      Points,  
      data=wine, 
      geom="point",
      alpha = 0.5) + # transparency = 50%
  scale_x_continuous(labels = dollar)


# Line chart: visualize relationship between 2 
# numeric variables and show local change 
# between points (X usually date/time)
# Example: average rating points by year
library(dplyr)
wine_grouped<-group_by(wine, Vintage)
vintage<-summarize(wine_grouped,
                   Total=n(),
                   Rating=mean(Points))
vintage # Average rating by year

#Let's create a line chart - good for variation over time




# Warning: some wines have missing vintage 

## Visualization with 2 Categorical Variables ##
# Heat map (2D bins) can show frequency by groups
# Example: total wines by type and continent

# Group countries into "Europe", "Oceania",
# "North America", and "South America"
wine$Continent<-as.factor(wine$Country)
levels(wine$Continent)[levels(wine$Continent)=="US"]<-
  "North America"
levels(wine$Continent)[levels(wine$Continent)=="Chile" |
                         levels(wine$Continent)=="Argentina"]<-"South America"
levels(wine$Continent)[levels(wine$Continent)=="Australia" |
                         levels(wine$Continent)=="New Zealand"]<-"Oceania"
levels(wine$Continent)[levels(wine$Continent)=="Spain" |
                         levels(wine$Continent)=="Portugal" |
                         levels(wine$Continent)=="Italy" |
                         levels(wine$Continent)=="Germany" |
                         levels(wine$Continent)=="France" |
                         levels(wine$Continent)=="Austria"]<-"Europe"
levels(wine$Continent)

qplot(Wine_Type,
      Continent,
      data=wine,
      geom="bin2d")

# Stacked bar chart: counts for 2 factors by 
# group and sub-group
qplot(Country,
      data=wine,
      geom="bar",       
      fill=Wine_Type) + # fill bars by type
  theme(axis.text.x = element_text(
    angle = 90, vjust=0.5)) # rotate labels 

# Facets: create separate plots for each 
# group with a shared axis
# Example: Total wines by type and continent
qplot(Wine_Type,         
      data=wine, 
      geom="bar", 
      facets= Continent~.)
# Each continent represented by facet on 
# vertical axis

# Might be easier to compare across 
# continents if facets are flipped and each 
# wine is assigned a color
qplot(Wine_Type,         
      data=wine, 
      geom="bar", 
      facets = . ~ Continent,
      fill=Wine_Type)

# VERY busy chart. Simplify by stripping x 
# axis labels (wine type is already 
# communicated by the legend/colors), and 
# moving legend
qplot(Wine_Type,         
      data=wine, 
      geom="bar", 
      facets= . ~ Continent,
      fill=Wine_Type) +
  theme(# remove x labels
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    # remove legend title
    legend.title=element_blank(), 
    # move legend
    legend.position="bottom")  

## 1 Continuous, 1 Categorical ##
# Compare numeric measure across groups with
# bar/column charts
# Example: compare average price by wine type
wine_grouped2<-group_by(wine, Wine_Type)
types<-summarize(wine_grouped2,
                 Total=n(),
                 AvgPrice=mean(Price))
types

qplot(Wine_Type,
      AvgPrice,
      data=types,
      geom="col") +
  scale_y_continuous(labels = dollar)

# Can also use "small multiples" and create 
# parallel plots (side by side)

# Parallel dot-plots: distribution by group
qplot(Wine_Type, # x variable (factor)
      Price, # y variable (numeric)
      data=wine,      
      geom="point") + # plot type
  scale_y_continuous(labels = dollar)

# Many overlapping points, use "jitter" to space
qplot(Wine_Type,      
      Price,          
      data=wine,      
      geom="jitter") + 
  scale_y_continuous(labels = dollar)

# Parallel box plots: distribution by group
qplot(Wine_Type, 
      Price, 
      data=wine, 
      geom="boxplot") +
  scale_y_continuous(labels = dollar, limits=c(0,75)) 

# Facets: create separate plots for each group
# with a shared axis
# Example: distribution of price by wine type
qplot(Price, # "x" variable
      data=wine, 
      geom="density", # plot type
      log="x", # log transform x variable
      xlab="Price (Log)",  
      facets=Wine_Type ~ .) 

# Fill: create overlapping plots for each 
# factor level
qplot(Price, 
      data=wine, 
      geom="density", 
      log="x", # log transform x variable
      fill=Wine_Type, # fill by type
      alpha=0.5)  # 50% transparency

# Add custom colors with scale_fill()
qplot(Price, 
      data=wine, 
      geom="density", 
      log="x", # log transform x variable
      fill=Wine_Type, # fill by type
      alpha=0.5) +
  scale_fill_manual(values=c("darkred", "salmon", 
                             "gold", "beige"))
