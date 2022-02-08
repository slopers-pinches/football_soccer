### Web scraping Top Goal Scorers from Africa Cup of Nations 2022


## 1) Load Libraries ----

library(tidyverse)
library(polite)
library(rlang)


## 2) Web Scrap Goal Scorers ----

# Define URL
url <- 'https://fbref.com/en/stathead/player_comparison.cgi?request=1&sum=0&comp_type=spec&dom_lg=1&spec_comps=656&player_id1=8637e09a&p1yrfrom=2021&player_id2=5ceba7fc&p2yrfrom=2021&player_id3=04bb2b06&p3yrfrom=2021&player_id4=b0c71810&p4yrfrom=2021&player_id5=f1e90c58&p5yrfrom=2021&player_id6=c691bfe2&p6yrfrom=2021&player_id7=40a800ef&p7yrfrom=2021'
# Random number for Delay
delay.sec <- floor(runif(1, min = 1, max = 60))
# Initialize Session
session <- polite::bow(url, user_agent = "AFCON Webscrape Operation", delay = delay.sec)

# Create tibble
afcon_22_top_scorers_tbl <- scrape(session) %>%
  rvest::html_nodes('table#standard_stats') %>%
  rvest::html_table(header = FALSE, trim = TRUE) %>%
  flatten_df() %>%
  # Select applicable columns & rows
  select(1,5,10) %>%
  slice(3:n()) %>%
  # Add column names
  set_names(c('Player_Name', 'National_Team', 'Total_Goals'))

# Save tibble as csv file
write_csv(afcon_22_top_scorers_tbl, 'data/AFCON_22_Top_Scorers.csv')

