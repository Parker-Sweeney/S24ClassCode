rm(list=ls())
###In this class, we'll use the cds.xml, oscars.xml file and example.html"

library(xml2)

?read_xml # Load XML data into R
cd<-read_xml("cds.xml")
oscars <-read_xml("oscars.xml")

children <- xml_children(cd)
siblings <- xml_siblings(children[[1]])
subchildren<-xml_children(children[[1]])


# Extract content within a given node
?xml_text # Return content from between start/end tags
xml_text(subchildren[[1]]) # Returns content of first tag (Title)

# Extract attribute values of a given node with xml_attr
?xml_attr
xml_attr(children[[1]], "year") # Returns value for "year" attribute

# How to get the name of every artist in one operation?
artists<-xml_text(xml_find_all(cd, "/CATALOG/CD/ARTIST"))
artists # Result is a character vector
length(unique(artists))
# Single slash / represents a direct path
# Use double slash // for relative paths (skip steps in the tree)
titles<-xml_text(xml_find_all(cd, "//TITLE"))
titles 
length(titles)
# Can also extract attribute values
years<-xml_attr(xml_find_all(cd, "//CD"), "year") 
years 
length(years)

# What if I want to store the years, titles, and artists
# as a data frame?
cd_df<-data.frame(years, titles, artists)


#XML Absolute Path#
movie_titles <- xml_text(xml_find_all(oscars,"/oscars/award/film/name"))

#XML Relative Path#
times <- xml_text(xml_find_all(oscars, "//time"))

oscars_df <- data.frame(movie_titles,times)

### HTML ###

## Parse HTML ##
library(xml2)
page<-read_html("example.html")
class(page) # Result is class xml_document
page # Print HTML to console

# Navigate tree structure
?xml_root # Return "root" node
xml_root(page) # <html> is root node

children<-xml_children(page)
children # First child node is <body>

subchildren<-xml_children(children[[1]])
subchildren # <body> contains 7 children:
# <h1>, <p>, <ul>, <h3>, <table>, <h3>, <table>

# Extract content from specific node
xml_text(subchildren[[1]]) # Extract text from <h1>

# More convenient to use XPath expressions to find
# nodes matching specific criteria
# Example: Locate each of the bulletted list items
items<-xml_find_all(page, "//ul/li")
class(items) # Result is class xml_nodeset
length(items) # 4 list items
items

# Extract just the content (no tags)
items<-xml_text(items)
items # Result is character vector

## Tables ##
# Tables are a common way of storing Web data
table<-xml_find_all(page, "//table")
length(table) # 2 tables on the page
# Sometimes you need to make XPaths more specific

# Assume we want the table of programming
# languages/tools. Notice that it has an id "tools"
table<-xml_find_all(page, "//table[@id='tools']")
length(table) # Only 1 table
table

# First column contains tool name. Extend the XPath
# to select the data in column 1 with [ ]
tools<-xml_find_all(page, "//table[@id='tools']/tr/td[1]")
length(tools)
tools 

# Extract the names as a vector
tools<-xml_text(tools)
tools

# First column also contains links <a>. Extend Xpath
# and extract with xml_attr()
urls<-xml_attr(xml_find_all(page, "//table[@id='tools']/
                            tr/td[1]/a"),"href")
length(urls) # Check if length is equal
urls

# Extract data from second column with [ ]
open_source<-xml_text(xml_find_all(page, "//table[@id='tools']/
                                   tr/td[2]"))
length(open_source)
open_source

# Combine vectors as a data frame:
tools_df<-data.frame(tool=tools, url=urls, open_source)
View(tools_df)
