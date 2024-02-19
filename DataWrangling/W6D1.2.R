###XML###
#Let's try writing our own .xml file
#Open up Visual Studio Code to create oscars.xml


## Parse XML ##
install.packages("xml2")
library(xml2)

?read_xml # Load XML data into R
cd<-read_xml("cds.xml")
oscars <-read_xml("oscars.xml")
class(cd) # Result is an object of xml_document class
cd # Print XML document to Console

## XML Trees ##
# XML has nested structure of parent-child nodes
?xml_children # Return all child nodes
children<-xml_children(cd)
class(children) # Result is an object of class xml_nodeset
children # Each CD is a child node

# Count number of children:
length(children) # 26

# Navigate to specific child node with [ ] or [[ ]]
children[[1]] # Result is object of xml_node class

# Navigate to siblings of current node 
?xml_siblings # Return all nodes at the same level
siblings<-xml_siblings(children[[1]])
class(siblings) # Result is an object of class xml_nodeset
length(siblings) # All CDs except the first
siblings 

# Navigate to children of current node (subchildren)
subchildren<-xml_children(children[[1]])
class(subchildren) # Result is an object of class xml_nodeset
length(subchildren) # Each CD has 5 tags
subchildren 

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
View(cd_df) 
str(cd_df) 
# Data extracted XML/HTML treated as text by default 
# Convert years to integer values
cd_df$years<-as.integer(cd_df$years)


## Conditional XPaths for Wednesday ##
# XPaths can also locate nodes based on attribute values with @
# Example: Find only CDs produced in the year 1990
nineties<-xml_find_all(cd, "//CD[@year='1990']")
nineties # Node set of 3 CDs produced in 1990

# Can also locate nodes based on content with text()
# Example: Find only CDs from Columbia
dylan_cds<-xml_find_all(cd, "//ARTIST[text()='Bob Dylan']")
dylan_cds # Node set of 1 <ARTIST> tag

# But how to return the title of Bob Dylan's CD?
# Navigate backwards in the tree with .. 
dylan_cds<-xml_find_all(cd, "//ARTIST[text()='Bob Dylan']/..")
dylan_cds # Returns node set of CDs where <ARTIST> tag matches 

# Can select just the title by extending XPath
dylan_cds<-xml_find_all(cd, "//ARTIST[text()='Bob Dylan']/../TITLE")
dylan_cds # Returns node set of <TITLE> tags where <ARTIST> 
# tag matches Bob Dylan


