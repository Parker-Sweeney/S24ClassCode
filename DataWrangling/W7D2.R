rm(list=ls())
### Web Crawling ###

## Loops ##
# Loops iterate over a set of data and apply the same 
# set of code to multiple inputs, one at a time

# While Loops #
# Executes as long as logical condition is TRUE
# Stops when condition becomes FALSE
age<-18 
while (age<21) { 
  print(age) # Must use print() to echo from inside loop
  print("Too young! No beer for you")
  age<-age+1 
}



# While loops are vulnerable to infinite loops
age<-18
while (age<21) { 
  print(age) # Must use print() to echo from inside loop
  print("Too young! No beer for you")
} # Hit STOP sign in Console to end loop

# Loop never ends because age is never increased

# For Loops #
# Iterates over a sequence and executes as many times
# as there are elements in the sequence

# Sequence of numbers

for (i in 1:5) { # Declare index i and its range (1-5)
  print(i) # Enumerate index and print its value
}               



# Vector 
letters <- c("a", "b", "c","d","e")
letters
for(i in letters) { print(i) }
#letters <- NULL

# NOTE: Do not have to put brackets on their own lines; 
# indentation optional

# Loop Control
# Break ends the loop (usually when a condition is met)
for(letter in letters) { # Declare index
  print(letter)
    if (letter == "b") # Conditional statement 
  { 
    print(letter) # If letter is b, print
    break # Then exit the loop
  }  
}  
# NOTE: Loops can be nested (within other loops or 
# conditional statements)

# Next jumps to the next iteration of the loop and skips 
# the remaining statements in the current iteration
for(i in 1:5) { # Declare index
 print(i)
    if (i<=2) # Conditional statement
     { 
      print(paste("skip second print of",i))
      next # If i is less than or equal to 2, skip
      
      }
          
  print(i) # Print i > 2
} 

## Web Crawling - Pagination ##
library(xml2) # Load package

# Quotes span 10 pages, each with a different URL 
# following the pattern ".../page/page_number"
# Scrape all quotes, author names, links, and keywords 
# and save in vectors
# Then construct a data frame with columns "Quote", 
# "Author", "Author_Url", "Tags"

rm(list=ls())

# Initialize empty vectors to store scraped data
all_quotes<-character(0) 
all_authors<-character(0) 
all_author_urls<-character(0)
all_tags<-character(0)

all_quotes
#Go to  https://www.whatismybrowser.com/detect/what-is-my-user-agent/
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
user_agent

start_time <- Sys.time()
start_time

for (i in 1:10){
  print(i)
  
 
  # Paste number (1, 2, 3, ...) at end of base URL
  url<-paste("http://quotes.toscrape.com/page/", i, sep="")
  # print(url)
  
  page<-read_html(url, user_agent)
 
  Sys.sleep(5) # Add 5-second wait time 
  
  page_quotes<-xml_text(xml_find_all(page,
             "//div[@class='quote']/span[@class='text']"))
  all_quotes<-c(all_quotes, page_quotes) # Append to quotes vector
  
  
  page_authors<-xml_text(xml_find_all(page,
              "//div[@class='quote']//small[@class='author']"))
  all_authors<-c(all_authors, page_authors) 
  
  
  page_author_urls<-xml_attr(xml_find_all(page, 
               "//div[@class='quote']/span/a"),
               "href")
  
  # Create full URL by concatenating domain
  page_author_urls<-paste("http://quotes.toscrape.com",
                          page_author_urls, sep="")
  
  all_author_urls<-c(all_author_urls, page_author_urls) 
  
  
  page_tags<-xml_attr(xml_find_all(page, 
                "//div[@class='quote']//meta"), 
                "content")
  all_tags<-c(all_tags, page_tags) 
  
  
  random_value <- sample(2:5, 1)
  print(paste("waiting ", random_value, " seconds"))
  Sys.sleep(random_value)

  print("moving to the next loop")
}
end_time <- Sys.time()
run_time <- end_time - start_time

print(paste("the program ran for ", run_time, " seconds"))


length(all_quotes)
length(all_authors)
length(all_tags)

all_quotes



# Combine vectors into data frame (can clean more later - that's what EoW is for)
quote_df<-data.frame(Quote=all_quotes, Author=all_authors, 
                     Author_Url=all_author_urls, Tags=all_tags)

# Write output to pipe-delimited file with UTF-8 encoding:
write.table(quote_df, "quote_info_crawled.txt", 
            sep="|", row.names=FALSE,
            fileEncoding="UTF-8")

