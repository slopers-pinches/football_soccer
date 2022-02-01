### Web Scraping 2020-2021 Big 5 European Leagues On Field Players (i.e. excluding goalkeepers) Stats ###

## 1) Load Libraries ----
library(tidyverse)
library(polite)
library(rlang)

## 2) Web Scraping Standard Stats ----

# Define URL
url <- "https://fbref.com/en/comps/Big5/2020-2021/stats/players/2020-2021-Big-5-European-Leagues-Stats"
# Random number for Delay
delay.sec <- floor(runif(1, min = 1, max = 60))
# Initialize Session
session <- polite::bow(url, user_agent = "Bryan's Webscrapping", delay = delay.sec)

# Web Scrape the Standard Stats table from fbref
Stand.2020.2021 <- scrape(session) %>%
  rvest::html_nodes('table#stats_standard')

# Create a Standard Stats df
Stand.Stats <- Stand.2020.2021 %>%
  rvest::html_table(header = FALSE, trim = TRUE) %>%
  flatten_df()

# Cleaning & Pre-processing
Stand.Stats <- Stand.Stats %>%
  slice(2:nrow(Stand.Stats)) %>%
  # Set the appropriate column names
  janitor::row_to_names(row_number = 1) %>%
  janitor::clean_names() %>%
  # Filter out the column names break
  filter(rk != 'Rk') %>%
  # Convert appropriate columns to numeric
  mutate(min = stringr::str_replace(min, ",", "")) %>%
  mutate_at(vars(-c(player, nation, pos, squad, comp, matches)), as.numeric) %>%
  # Clean up Nation and Comp
  mutate(nation = stringr::str_extract(nation, pattern = "[:upper:]+"),
         comp = case_when(comp == "eng Premier League" ~ "English Premier League",
                          comp == "fr Ligue 1" ~ "Ligue 1",
                          comp == "it Serie A" ~ "Serie A",
                          comp == "de Bundesliga" ~ "Bundesliga",
                          comp == "es La Liga" ~ "La Liga")) %>%
  # Rename certain columns
  rename(gls_per_90 = gls_2,
         ast_per_90 = ast_2,
         gls_ast_per_90 = g_a,
         gls_minus_ast_per_90 = g_pk_2,
         gls_plus_ast_minus_pk_per_90 = g_a_pk,
         xg = x_g,
         npxg = npx_g,
         xa = x_a,
         npxg_plus_xa = npx_g_x_a,
         xg_per_90 = x_g_2,
         xa_per_90 = x_a_2,
         xg_plus_xa_per_90 = x_g_x_a,
         npxg_per_90 = npx_g_2,
         npxg_plus_xa_per_90 = npx_g_x_a_2) %>%
  # Remove irrelevant columns
  select(-c(rk, matches))

# Save Stand.Stats as a csv file
write_csv(Stand.Stats, file = 'Standard_Stats_2020_2021.csv')




## 3) Web Scraping Shooting Stats ----

# Define URL
url <- "https://fbref.com/en/comps/Big5/2020-2021/shooting/players/2020-2021-Big-5-European-Leagues-Stats"
# Random number for Delay
delay.sec <- floor(runif(1, min = 1, max = 60))
# Initialize Session
session <- polite::bow(url, user_agent = "Bryan's Webscrapping", delay = delay.sec)

# Web Scrape the Shooting Stats table from fbref
Shooting.2020.2021 <- scrape(session) %>%
  rvest::html_nodes('table#stats_shooting')

# Create a Shooting Stats df
Shooting.Stats <- Shooting.2020.2021 %>%
  rvest::html_table(header = FALSE, trim = TRUE) %>%
  flatten_df()

# Cleaning & Pre-processing
Shooting.Stats <- Shooting.Stats %>%
  slice(2:nrow(Shooting.Stats)) %>%
  # Set the appropriate column names
  janitor::row_to_names(row_number = 1) %>%
  janitor::clean_names() %>%
  # Filter out the column names break
  filter(rk != 'Rk') %>%
  # Convert appropriate columns to numeric
  mutate_at(vars(-c(player, nation, pos, squad, comp, matches)), as.numeric) %>%
  # Clean up Nation and Comp
  mutate(nation = stringr::str_extract(nation, pattern = "[:upper:]+"),
         comp = case_when(comp == "eng Premier League" ~ "English Premier League",
                          comp == "fr Ligue 1" ~ "Ligue 1",
                          comp == "it Serie A" ~ "Serie A",
                          comp == "de Bundesliga" ~ "Bundesliga",
                          comp == "es La Liga" ~ "La Liga")) %>%
  # Rename certain columns
  rename(per_90 = x90s,
         sh_per_90 = sh_90,
         so_t_per_90 = so_t_90,
         g_per_sh = g_sh,
         g_per_so_t = g_so_t,
         npxg_per_sh = npx_g_sh,
         g_minus_xg = g_x_g,
         npg_minus_xg = np_g_x_g) %>%
  # Remove irrelevant columns
  select(-c(rk, pos, matches))

# Save Shooting.Stats as a csv file
write_csv(Shooting.Stats, file = 'Shooting_Stats_2020_2021.csv')




## 4) Web Scraping Passing Stats ----

# Define URL
url <- "https://fbref.com/en/comps/Big5/2020-2021/passing/players/2020-2021-Big-5-European-Leagues-Stats"
# Random number for Delay
delay.sec <- floor(runif(1, min = 1, max = 60))
# Initialize Session
session <- polite::bow(url, user_agent = "Bryan's Webscrapping", delay = delay.sec)

# Web Scrape the Passing Stats table from fbref
Passing.2020.2021 <- scrape(session) %>%
  rvest::html_nodes('table#stats_passing')

# Create a Passing Stats df
Passing.Stats <- Passing.2020.2021 %>%
  rvest::html_table(header = FALSE, trim = TRUE) %>%
  flatten_df()

# Cleaning & Pre-processing
Passing.Stats <- Passing.Stats %>%
  slice(2:nrow(Passing.Stats)) %>%
  # Set the appropriate column names
  janitor::row_to_names(row_number = 1) %>%
  janitor::clean_names() %>%
  # Filter out the column names break
  filter(rk != 'Rk') %>%
  # Convert appropriate columns to numeric
  mutate_at(vars(-c(player, nation, pos, squad, comp, matches)), as.numeric) %>%
  # Clean up Nation and Comp
  mutate(nation = stringr::str_extract(nation, pattern = "[:upper:]+"),
         comp = case_when(comp == "eng Premier League" ~ "English Premier League",
                          comp == "fr Ligue 1" ~ "Ligue 1",
                          comp == "it Serie A" ~ "Serie A",
                          comp == "de Bundesliga" ~ "Bundesliga",
                          comp == "es La Liga" ~ "La Liga")) %>%
  # Rename certain columns
  rename(per_90 = x90s,
         short_comp = cmp_2,
         short_att = att_2,
         short_comp_percent = cmp_percent_2,
         medium_comp = cmp_3,
         medium_att = att_3,
         medium_comp_percent = cmp_percent_3,
         long_cmp = cmp_4,
         long_att = att_4,
         long_cmp_percent = cmp_percent_4,
         xa = x_a,
         a_minus_xa = a_x_a,
         cmp_pass_.33_pitch = x1_3,
         prog_passes = prog) %>%
  # Remove irrelevant columns
  select(-c(rk, pos, matches))

# Save Passing.Stats as a csv file
write_csv(Passing.Stats, file = 'Passing_Stats_2020_2021.csv')




## 5) Web Scraping Possession Stats ----

# Define URL
url <- "https://fbref.com/en/comps/Big5/2020-2021/possession/players/2020-2021-Big-5-European-Leagues-Stats"
# Random number for Delay
delay.sec <- floor(runif(1, min = 1, max = 60))
# Initialize Session
session <- polite::bow(url, user_agent = "Bryan's Webscrapping", delay = delay.sec)

# Web Scrape the Standard GK Stats table from fbref
Poss.2020.2021 <- scrape(session) %>%
  rvest::html_nodes('table#stats_possession')

# Create a Possession Stats df
Poss.Stats <- Poss.2020.2021 %>%
  rvest::html_table(header = FALSE, trim = TRUE) %>%
  flatten_df()

# Cleaning & Pre-processing
Poss.Stats <- Poss.Stats %>%
    slice(2:nrow(Poss.Stats)) %>%
    # Set the appropriate column names
    janitor::row_to_names(row_number = 1) %>%
    janitor::clean_names() %>%
    # Filter out the column names break
    filter(rk != 'Rk') %>%
    # Convert appropriate columns to numeric
    mutate_at(vars(-c(player, nation, pos, squad, comp, matches)), as.numeric) %>%
    # Clean up Nation and Comp
    mutate(nation = stringr::str_extract(nation, pattern = "[:upper:]+"),
           comp = case_when(comp == "eng Premier League" ~ "English Premier League",
                            comp == "fr Ligue 1" ~ "Ligue 1",
                            comp == "it Serie A" ~ "Serie A",
                            comp == "de Bundesliga" ~ "Bundesliga",
                            comp == "es La Liga" ~ "La Liga")) %>%
    # Rename certain columns
    rename(per_90 = x90s,
           prog_.33_pitch = x1_3,
           prog_passes_rec = prog_2) %>%
    # Remove irrelevant columns
    select(-c(rk, pos, matches))

# Save Poss.Stats as a csv file
write_csv(Poss.Stats, file = 'Poss_Stats_2020_2021.csv')