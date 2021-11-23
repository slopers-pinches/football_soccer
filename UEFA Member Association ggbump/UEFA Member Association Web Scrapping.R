### Web scrapping for UEFA Member Association Club Coefficients ###

# Load Libraries
library(tidyverse)
library(polite)
library(rlang)

## Web Scrapping ##
# Define URL
url <- "https://en.wikipedia.org/wiki/UEFA_coefficient#2017_ranking"
# Random number for Delay
delay.sec <- floor(runif(1, min = 1, max = 60))
# Initialize Session
session <- polite::bow(url, user_agent = "Bryan's Webscrapping", delay = delay.sec)

# Web Scrape the table from Wiki
uefa.coeff <- scrape(session) %>%
                rvest::html_nodes('table:nth-child(61)')

# Convert to df
uefa.df <- uefa.coeff %>%
                rvest::html_table(header = FALSE, trim = TRUE) %>%
                flatten_df()

## Clean-up & Pre-processing ##
uefa.df <- uefa.df[2:nrow(uefa.df), ]
uefa.df <- janitor::row_to_names(uefa.df, row_number = 1)
# Select relevant columns
uefa.df <- uefa.df[ , 4:9]

uefa.df <- uefa.df %>%
              # Convert to numerical values
              mutate_at(vars(-`Member association(L: League, C: Cup, LC: League cup[a])`),
                        funs(as.numeric(.))) %>%
              rename(UEFA_Country = `Member association(L: League, C: Cup, LC: League cup[a])`) %>%
              # Extract country name & Remove any whitespaces
              mutate(UEFA_Country = str_trim(str_extract(UEFA_Country, pattern = '^[A-Za-z\\s]+'),
                                             side = 'both'))

# Preview of dataframe
head(uefa.df)

# Check for any missing values or N/A's
any(is.na(uefa.df)) # <-- NONE

# Save it as a csv.file
write_csv(uefa.df, file = 'uefa_member_association_coeff.csv')
