rm(list=ls())
### Web Scraping ###

## Parse HTML ##
#install.packages("xml2")
library(xml2)

#Go to  https://www.whatismybrowser.com/detect/what-is-my-user-agent/
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
user_agent

current_page <- "http://quotes.toscrape.com/"
current_page

page<-read_html(current_page, user_agent)
Sys.sleep(5) # in seconds

class(page) # Result is class xml_document
page # Print HTML to console

# Data is arranged in <div> tags with class "quote"
divs<-xml_find_all(page, "//div[@class='quote']")
class(divs)
length(divs)

# Examine first div
divs[[1]] 
# Quote text located in a <span> tag with class "text"
# Other data (author name, link to author bio, tags) enclosed 
# in other tags

### Scrape quote text:
# Looks for span tag within the quote divs that has class=text (since it is unique to where the quotes are)
quotes<-xml_text(xml_find_all(page,"//div[@class='quote']/span[@class='text']"))
# Looks for span tag that has itemprop=text (since it is unique to where the quotes are)
quotes2<-xml_text(xml_find_all(page,"//span[@itemprop='text']"))

length(quotes) # Returns vector of 10 quotes
quotes
# Notice curly quotes. Scraped data will need to be cleaned
quotes <- gsub('“','',quotes)
quotes <- gsub('”','',quotes)
quotes[1]

# Author names enclosed in <small> tag with class "author"
authors<-xml_text(xml_find_all(page,"//div[@class='quote']/small[@class='author']"))
length(authors) # Returns 0
# <small> is not the direct child of <div>

# Answer: Use relative path //
authors<-xml_text(xml_find_all(page,"//div[@class='quote']//small[@class='author']"))
length(authors) # Returns vector of 10 authors
authors
# Notice Andre Gide's name. Data contains special characters

# Hyperlink to author biography after the <small> tag
# Can use .. to move backward in tree or following-sibling to go forward
urls<-xml_attr(xml_find_all(page,"//div[@class='quote']
              //small[@class='author']/following-sibling::a"),
              "href")
length(urls) # Returns vector of 10 URLs
urls 
# Notice that links are relative (not full URL)

# Quotes have multiple keywords in a <div> with class "tags"
# Tags can be extracted in one operation from <meta> 
keywords<-xml_attr(xml_find_all(page,
          "//div[@class='tags']//meta[@class='keywords']"),
          "content")
length(keywords) # Returns vector of 10 sets of tags
keywords 

# Because all vectors have equal length, can transform
# to data frame
quote_df<-data.frame(quotes, authors, urls, keywords)

## Clean Scraped Data ##
# Strip curly quotes
# quote_df$quotes[6:10]
# quote_df$quotes <- gsub("â", "", quote_df$quotes)
# quote_df$quotes <- gsub("â", "", quote_df$quotes)
# 
# quote_df$quotes[1:5]

#quote_df$quotes<-gsub("\"", "", quote_df$quotes)

# Create full urls by adding top-level domain
quote_df$urls[1:5]
quote_df$urls<-paste("https://quotes.toscrape.com",
                     quote_df$urls, sep="")
quote_df$urls[1:5]


quote_df$authors <- trimws(quote_df$authors)


# Write output to pipe-delimited file 
# Use fileEncoding argument to keep special characters
write.table(quote_df, "quotes_scraped.txt", 
            sep="|", row.names=FALSE, 
            fileEncoding="UTF-8")

# Or transliterate (transform special characters)
# before writing to file
?iconv
quote_df$authors<-iconv(quote_df$authors, 
                        from="UTF-8",
                        to="ASCII//TRANSLIT",
                        sub="?")
write.table(quote_df, "quotes_scraped2.txt",
            sep="|", row.names=FALSE)

# NOTE: Transliterated results may appear slightly 
# different on Macs vs. PCS
