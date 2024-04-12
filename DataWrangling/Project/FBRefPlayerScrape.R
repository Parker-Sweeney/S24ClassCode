# Load the required libraries
library(rvest)
library(httr)
library(xml2)
library(lubridate)

# Set the URL of the page to scrape
currentpage <- 'https://fbref.com/en/players/2b09d998/Tyler-Adams'

# Set user agent to simulate a browser visit
user_agent <- 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36'

# Start a session
page <- read_html(currentpage, user_agent)

# Extract the player's full name
player_name <- xml_text(xml_find_all(page, "//h1/span"))
player_bd <- as.Date(trimws(xml_text(xml_find_all(page, "//span[@id='necro-birth']"))), format="%B %d, %Y")
player_bp <- trimws(xml_text(xml_find_all(page, "//p/span")))[4]
player_footed <- trimws(xml_text(xml_find_all(page, "//strong[contains(text(), 'Footed:')]/following-sibling::text()")))
player_age <- floor(as.numeric(difftime(Sys.Date(), player_bd, units = "days"))/365.25)

# Combine the variables into a named list
player_data <- list(
  Name = player_name,
  Birthdate = player_bd,
  Age = player_age,
  Birthplace = player_bp,
  Footed = player_footed
)

# Convert the list to a data frame
player_df <- as.data.frame(player_data)
