library(rvest)
library(xml2)
library(chromote)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)

# Set the user agent string
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"

# Define the URL
url <- "https://www.besoccer.com/team/squad/seleccion-estados-unidos"

# Read the HTML content from the URL
page <- read_html(url)

# Get Links

player_links <- xml_attr(xml_find_all(page, "//td[contains(@class, 'name')]/a"),"href")
player_names <- xml_text(xml_find_all(page, "//td[contains(@class, 'name')]/a"))

links_df <- data.frame(
  Name = player_names,
  Link = player_links,
  stringsAsFactors = FALSE
)

###################################

final_df <- data.frame(
  Name = character(0),
  Season = character(0),
  Pts = numeric(0),
  ELO = numeric(0),
  stringsAsFactors = FALSE
)

urls <- player_links[1:3]
names <- player_names[1:3]

url <- player_links[39]

for(url in urls) {
  print(url)
    
  # Read the HTML content from the URL
  page <- read_html(url)
  
  # Navigate to the table
  table <- html_node(page, "table.table_parents")
  
  # Extract rows with the class 'parent_row'
  rows <- html_nodes(table, "tr.row-body.parent_row.color-dark")
  
  # Extract values from the specified columns
  year_id <- html_attr(rows, "id")
  #age_values <- html_text(html_nodes(rows, "td:nth-child(12)"))
  pts_values <- html_text(html_nodes(rows, "td:nth-child(13)"))
  elo_values <- html_text(html_nodes(rows, "td:nth-child(14)"))
  player_name <- html_text(html_nodes(rows, ".ta-c div.panel-subtitle"))
  
  # Create a data frame with the extracted values
  base_df <- data.frame(
    Season = year_id,
    Pts = as.numeric(pts_values),
    ELO = as.numeric(elo_values),
    stringsAsFactors = FALSE
  )
  
  split_df <- base_df %>%
    separate(Season, into = c("Team", "Season"), sep = "(-(?=\\d{4}/\\d{2}))", extra = "merge")
  
  indv_player_stats_df <- split_df %>%
    group_by(Season) %>%
    summarise(
      Team = first(Team),
      Avg_Pts = mean(Pts),
      Avg_ELO = mean(ELO),
      .groups = 'drop'
    ) %>%
    arrange(desc(Season))
  
  indv_player_stats_df$PlayerName <- player_name
}
