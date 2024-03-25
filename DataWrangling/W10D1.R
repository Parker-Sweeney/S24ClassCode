#Class 26 Blank
mortgage<-read.csv("mortgage.txt",
                   sep="|", 
                   na.strings=c("NA","-",""),
                   colClasses=c("integer", 
                                "integer","integer", 
                                "integer","integer", 
                                "factor"))

# Remove rows with missing values:
mortgage<-na.omit(mortgage)



## Statistics for Two Continuous Variables ##
# Correlation measures strength of linear 
# relationship
?cor # Default is Pearson correlation 
# (Kendall or Spearman may be appropriate 
# for comparing numeric ranks)
cor(mortgage$CreditScore, mortgage$HomeAge)
# Interpretaion: no correlation

?cor.test # H0=no correlation, H1=correlation!=0
ct<-cor.test(mortgage$CreditScore, 
             mortgage$CreditCardDebt)

class(ct) # Object of class "hypothesis test"
ct$p.value
# Interpretation: p > 0.05, thus we fail to
# reject the null hypothesis that the 
# correlation is 0

# Visualize relationships between continuous
# variables with scatterplot:
?plot # Create scatterplots and line charts
plot(mortgage$CreditScore, # x axis
     mortgage$HomeAge, # y axis
     xlab="Credit Score", # x axis title
     ylab="CreditCardDebt") # y axis title



# Load "allstate.txt"
allstate<-read.csv("allstate.txt", 
                   na.strings=c(""), colClasses=c("integer",
                                                  "integer", "factor", "factor", "integer"))

#Create a linear regression modeling cost as a
# function of car age
cost_model<-lm(cost~car_age,
               data=allstate)

# Interpret the regression results
summary(cost_model)
# Intercept: 6-month cost is estimated to be $637.57


## Read Data ##
students<-read.csv("students_math.csv",
                   stringsAsFactors=TRUE)
str(students) # Math class grades for
# 395 students at two Portuguese schools
# 33 variables tracking students' sex,
# age, family information, previous 
# academic performance, etc.

## Linear Regression ##
# Is the final math grade related to the
# number of previous failures?

?lm # Fit linear models based on formula
fit<-lm(final~failures, data=students)
summary(fit)
# Intercept: Final grade is estimated to be
# 11.2 (out of 20) when the student has no
# previous failures
# Failures: Each additional failure is
# associated with a decrease of 2.2 points 
# on the studentâs final grade 
# Number of failures is a significant predictor 
# of final grade
# R-squared: Number of failures explains
# 13% of the variation in final grades
# F-test: Reject the null hypothesis that
# that the null model and current model have
# equally good fit

## Check Assumptions ##
# Linearity: correlation
cor(students$final, students$failures)
# Weak/moderate linear relationship

# Visualize relationship with scatterplot:
plot(final~failures, data=students)
# Add regression line with abline()
abline(fit, col="red")

# Normality: Q-Q plot of residuals
qqnorm(fit$residuals)
qqline(fit$residuals, col="red")
# Marks deviate from reference line
# Violation does not mean results are useless,
# but coefficient sizes and p-values may be 
# inaccurate

# Homoscedasticity: plot fitted values vs. 
# residuals
plot(fit$fitted.values, fit$residuals,
     xlab="Fitted Values", ylab="Residuals")
# Plot has a cone shape (variation is low 
# on the left, but high on the right)
# Violation of variance assumption is
# more serious, and may indicate that a
# different model is more appropriate 

## Multiple Regression ##
# Is the final math grade related to number
# of failures and mother's education?
fit2<-lm(final~failures+m_edu, data=students)
summary(fit2)
# Intercept: Final grade is estimated 
# to be 9.5 when a student has no failures
# and mother has no education
# Failures: Each additional failure is 
# associated with a decrease of 2 points
# M_edu: Each additional level of education
# is associated with an increase of .9 points
# Adjusted R-squared: Failures and mother's
# education explain 14% of the variation in 
# final grades
# F-test: Reject the null hypothesis that the null 
# model and current model have equally good fit

# Check multicollinearity
cor(students$failures, students$m_edu)
plot(students$failures, students$m_edu,
     xlab="Failures", ylab="Mother's Education")
# No evidence of correlation between 2 variables




## Load Data ##
# Load wine ratings data:
wine<-read.csv("wine_ratings.txt", sep="|")
str(wine)
# Data on 14,781 wine ratings including 
# variety, winery, country, price, and rating

## ggplot2 ##
# install.packages("scales")
library(scales) # Format labels, legends, etc.
# install.packages("ggplot2)
library(ggplot2)  # Create "elegant" visualizations

## Visualization with 1 Continuous Variable ##
# Histogram: visualize distribution of one 
# numeric  variable
# "Base R" histogram:
hist(wine$Price) 
# Not pretty, but OK for checking distribution

# ggplot2 histogram:
?qplot # Quick plot
qplot(Price, # x axis data
      data=wine, # data source
      geom="histogram") # plot type
# qplot() will produce a warning because we 
# did not specify number of bins or bin width 
# (30 bins is default)

# Rule of thumb for histograms: bins ~ sqrt(n)
sqrt(nrow(wine))
qplot(Price,         
      data=wine,          
      geom="histogram",
      bins=120) # bins

# Or specify bin width:
qplot(Price,         
      data=wine,          
      geom="histogram",
      binwidth = 20, # $20 intervals
      xlab="Wine Price ($)") # axis label

# Use scales functions to sort, reverse, and 
# format axes values
qplot(Price,         
      data=wine,          
      geom="histogram",
      binwidth=20, # $20 interval
      xlab="Sale Price") +  # add scale with +
  scale_x_continuous(labels = dollar)    

# Density plot: visualize distribution of one 
# numeric variable (continuous)
qplot(Price, 
      data=wine, 
      geom="density", # plot type
      color=I("blue")) # set line color
# ggplot2 colors: 
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

# Boxplot: visualize spread of one numeric 
# variable
qplot("",  # group (empty)
      Price, # measure
      data=wine, # data
      geom="boxplot") + 
  scale_y_continuous(labels = dollar)

# Violin plot: visualize spread and 
# distribution of one numeric variable. 
# Combines box plot and density plot 
# (width represents density)
qplot("",  # group (empty)
      Price, # measure
      data=wine, # data
      geom="violin") + 
  scale_y_continuous(labels = dollar)


