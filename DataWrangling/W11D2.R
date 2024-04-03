#Class 30 Blank
rm(list=ls())
#More Map Visualizations
dsm<-read.csv("desmoines_housing_GIS.csv")
str(dsm)
# Data on 5147 house sales including city, date,
# price, address, zip code, coordinates, etc.

# Load ggplot2 package:
library(ggplot2)
library(scales)
## Coordinate Map ##
# Create scatterplot with longitude and latitude
# as XY coordinates 
qplot(lon, lat, data=dsm, geom="point", col=city)

# Add arguments to color marks by city


# Fill marks by group

# Many similar colors, could be helpful to 
# focus on subset of 4 cities
dsm_subset<-subset(dsm, city=="Des Moines" |
                     city=="West Des Moines" |
                     city=="Altoona" |
                     city=="Urbandale")
qplot(lon, 
      lat, 
      data=dsm_subset, 
      geom="point",
      color=city)+ # Fill marks by group
  theme_bw()+
  theme(legend.title=element_blank(), 
        legend.position="bottom")



### High-Dimensional Visualization ###

## Load Data and Packages ##
# Load wine ratings data:
wine<-read.csv("wine_ratings.txt", sep="|")
# Data on 14,781 wine ratings including variety,
# winery, country, price, reviewer, and rating

# Load scales and ggplot2 packages:
library(scales)
library(ggplot2)

## Visualization with 3 Variables ##
## 2 Continuous, 1 Categorical ##
# Multiple line chart: visualize trends for multiple
# numeric variables, grouped into series by factor
# Example: Average ratings by vintage (year) with 
# separate lines for each wine type
library(dplyr)
wine_grouped<-group_by(wine, Vintage, 
                       Wine_Type)
vintage_type<-summarize(wine_grouped,
                        Rating=mean(Points))
vintage_type # summary by year and type

qplot(Vintage, # x axis
      Rating, # y axis
      data=vintage_type, 
      geom="line", col=Wine_Type)  #Lastly, we want to have the color be wine type
# Warning: some wines missing vintage data

# Parallel scatter plots: compare relationship
# between 2 numeric variables by factor
# Example: Compare relationship between 
# price and points for US. vs. French wines
wine_subset<-subset(wine, 
                    Country=='US' | Country=='Spain')
qplot(Price, 
      Points, 
      data=wine_subset, 
      geom="point",
      facets= . ~ Country) + 
  scale_x_continuous(labels = dollar)

# Can also use one multi-series scatter plot
qplot(Price, 
      Points, 
      data=wine_subset, 
      geom="point", 
      shape=Country) + # Points shape 
  scale_x_continuous(labels = dollar)

# Not very readable. Use colors to differentiate
qplot(Price, 
      Points, 
      data=wine_subset, 
      geom="point",
      shape=Country,
      color=Country, # mark color
      alpha=0.5) + 
  scale_x_continuous(labels = dollar)

## 2 Categorical, 1 Continuous ##
# Parallel bar/column charts can compare numeric
# measure by 2 factors
# Example: Display average price for each wine
# type with parallel plots for US vs. France
wine_subset_grouped<-group_by(wine_subset,
                              Wine_Type,
                              Country)
types<-summarize(wine_subset_grouped,
                 AvgPrice=mean(Price))
types 

qplot(Wine_Type,
      AvgPrice,
      data=types,
      geom="col",
      facets=.~Country)+
  scale_y_continuous(labels = dollar)+
  theme(axis.text.x = element_text(
    angle = 90, vjust=0.5))

# Could also use one clustered column chart
# (more customization options with ggplot())
ggplot(types, 
       aes(x=Wine_Type, y=AvgPrice, 
           fill=Country)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_y_continuous(labels = dollar)+
  theme(axis.text.x = element_text(
    angle = 90, vjust=0.5))

## 3 Continuous Variables ##
# Can use color, transparency, and size to
# express a third numeric varaible
# Example: Use a scatterplot to visualize relationship
# between price and ratings, with consideration of vintage
qplot(Price, 
      Points, 
      data=wine, 
      geom="point", 
      color=Vintage) + # color by year
  scale_x_continuous(labels = dollar)
# Color is expressed as gradient (dark to light)
# because Vintage is continuous
# Gray marks for wines with unknown vintage

# Transparency can also be linked to numeric variable
qplot(Price, 
      Points, 
      data=wine, 
      geom="point", 
      alpha=Vintage) + # transparency 
  scale_x_continuous(labels = dollar)
# Not very interpretable due to overlapping marks


## 3 Categorical Variables ##
# Stacked bar/column chart with facets can express
# frequency based on 3 factors
# Example: Create 2 stacked column charts for
# US vs. France. Each column represents the total 
# number of wines by type with segments for price point
# (Low: $0-40, Mid: $40-100, High: $100+). 
wine_subset$PricePoint<-cut(wine_subset$Price,
                            breaks=c(-Inf,39,99,Inf),
                            labels=c("Low","Mid","High"))
levels(wine_subset$PricePoint)

qplot(Wine_Type,
      data=wine_subset,
      geom="bar",
      fill=PricePoint,
      facets = .~Country)+
  theme(axis.text.x = element_text(
    angle = 90, vjust=0.5))

# Can visualize same data as 2D grid (matrix)
# of column charts
qplot(Wine_Type,
      data=wine_subset,
      geom="bar",
      facets=PricePoint~Country) +
  # Create vertical and horizontal facets
  theme(axis.text.x = element_text(
    angle = 90, vjust=0.5))

## Visualization with Many Variables ##
# 4 variables: scatterplot matrix comparing 
# relationship between price and ratings by country 
# (US, France) and wine type
qplot(Price, 
      Points, 
      data=wine_subset, 
      geom="point", 
      facets= Country~Wine_Type) + 
  scale_x_continuous(labels = dollar) +
  theme(axis.text.x = element_text(
    angle = 90, vjust=0.5))
