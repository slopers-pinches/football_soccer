#### Ternary Plots of On-Field Football Players ####

# Objective:
# - How did the club's squad perform in the league during the 2020–21 season?
# - How was the player's performance compare to his fellow teammates during the 2020–21 season?
# - During the 2020–21 season, how do the 2 clubs performed from each other?
# - How did the club's players performed during the 2020–21 season?


## 1) Load Libraries ----
library(tidyverse)
library(ggtern)
library(plotly)

## 2) Import Data of On-Field Stats Season 2020-21 ----

standard_stats_tbl <- read_csv("data/Standard_Stats_2020_2021.csv")
shooting_stats_tbl <- read_csv("data/Shooting_Stats_2020_2021.csv")
poss_stats_tbl <- read_csv("data/Poss_Stats_2020_2021.csv")
passing_stats_tbl <- read_csv("data/Passing_Stats_2020_2021.csv")

## 3) Data Pre-processing ----

# 3.1) Data Clean-up ----

# During the Season 2020-21, many players were loaned or transferred from 1 club to another.
# Example: Luka Jovic (Real Madrid --> Eintracht Frankurt) & Martin Ødegaard (Real Madrid --> Arsenal)
# There are duplicate players; so, stats must be added together to measure players' performance.
# Also, the latest club & league will be the current for players at the end of the season.
# The highest x90s value will be the player's latest club & league.

# Duplicates Records of Players
standard_stats_tbl %>%
  select(player, nation, age) %>%
  group_by(player, nation, age) %>%
  # Flag more than 1 occurrence
  mutate(dupe = n() > 1) %>%
  ungroup() %>%
  # Filter TRUE
  filter(dupe == TRUE) %>%
  group_by(player, nation, age) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  # Check players' transfer activity
  View()


# Eliminate as many duplicate players and select the most recent club and league
players_stats_tbl <- standard_stats_tbl %>%
  select(player, nation, age, pos, squad, comp, x90s) %>%
  # Flag the max x90s
  group_by(player, nation, age) %>%
  mutate(max_x90s = max(x90s)) %>%
  ungroup() %>%
  mutate(keep_row = case_when(
                          max_x90s == x90s ~ TRUE,
                          TRUE ~ FALSE
  )) %>%
  # Filter TRUE Only to avoid duplicate players
  filter(keep_row == TRUE) %>%
  # Keep relevant columns
  select(player, nation, age, pos, squad, comp)


# Double-check for duplicate players (manual process)
dup_players <- players_stats_tbl[duplicated(players_stats_tbl$player), ] %>%
                    select(player) %>%
                    pull()
# There are 2 duplicate players (Arturo Calabresi & Cristiano Piccini).
# When they were transferred/loaned out, they barely played like their previous club. (quick Google check)
players_stats_tbl %>%
  filter(player %in% dup_players) %>%
  arrange(player) %>%
  View()


# Manually select their most recent club.
# Filter out: Arturo Calabresi - Bologna & Cristiano Piccini - Valencia
players_stats_tbl <- players_stats_tbl %>%
  filter(!(player %in% c('Arturo Calabresi', 'Cristiano Piccini') & squad %in% c('Bologna', 'Valencia')))

players_stats_tbl

# 3.2) Calculation ----

# Match Played 
mp_tbl <- standard_stats_tbl %>%
  select(player, nation, age, mp) %>%
  group_by(player, nation, age) %>%
  summarise(total_mp = sum(mp)) %>%
  ungroup()

# Passing
passing_tbl <- passing_stats_tbl %>%
  select(player, nation, age, cmp, att) %>%
  group_by(player, nation, age) %>%
  # Add up the pass completion and attempt
  summarise(total_cmp = sum(cmp),
            total_att = sum(att)) %>%
  ungroup() %>%
  # Calculate the pass percentage
  mutate(pass_pct = round((total_cmp / total_att), 3) * 100) %>%
  # Replace NaN
  mutate(pass_pct = ifelse(is.nan(pass_pct), 0, pass_pct)) %>%
  select(-c(total_cmp, total_att))

# Shooting
shooting_tbl <- shooting_stats_tbl %>%
  select(player, nation, age, sh, so_t) %>%
  group_by(player, nation, age) %>%
  # Add up the shot attempts and shot on target
  summarise(total_shot = sum(sh),
            total_shot_target = sum(so_t)) %>%
  ungroup() %>%
  # Calculate the shot percentage
  mutate(shot_pct = round((total_shot_target / total_shot), 3) * 100) %>%
  # Replace NaN
  mutate(shot_pct = ifelse(is.nan(shot_pct), 0, shot_pct)) %>%
  select(-c(total_shot, total_shot_target))

# Dribbling
dribbling_tbl <- poss_stats_tbl %>%
  select(player, nation, age, succ, att) %>%
  group_by(player, nation, age) %>%
  # Add up the dribbling attempts and successes
  summarise(total_att = sum(att),
            total_succ = sum(succ)) %>%
  ungroup() %>%
  # Calculate the dribbling percentage
  mutate(dribbling_pct = round((total_succ / total_att), 3) * 100) %>%
  # Replace NaN
  mutate(dribbling_pct = ifelse(is.nan(dribbling_pct), 0, dribbling_pct)) %>%
  select(-c(total_att, total_succ))

# 3.3) Merging ----

stats_2020_21_tbl <- players_stats_tbl %>%
  left_join(mp_tbl, by = c("player", "nation", "age")) %>%
  left_join(passing_tbl, by = c("player", "nation", "age")) %>%
  left_join(dribbling_tbl, by = c("player", "nation", "age")) %>%
  left_join(shooting_tbl, by = c("player", "nation", "age"))

stats_2020_21_tbl


# 4) Ternary Plots ----

# 4.1) Compare Club's Squad to its respective League ----

fc_bayern <- stats_2020_21_tbl %>%
  # Filter out goalkeepers
  # Filter for Bayern Munich
  filter(squad == 'Bayern Munich' & pos != 'GK') %>%
  rename(Passing = pass_pct,
         Dribbling = dribbling_pct,
         Shooting = shot_pct,
         League = comp)

stats_2020_21_tbl %>%
  # Filter out goalkeepers
  # Filter for Bundesliga
  filter(comp == 'Bundesliga' & pos != 'GK') %>%
  rename(Passing = pass_pct,
         Dribbling = dribbling_pct,
         Shooting = shot_pct,
         League = comp) %>%
  ggtern(aes(x = Passing, y = Dribbling, z = Shooting)) +
    geom_point(size = 3,
               shape = 21,
               color = 'grey',
               fill = 'grey') +
  geom_point(data = fc_bayern,
             aes(x = Passing, y = Dribbling, z = Shooting),
             size = 3,
             shape = 21,
             fill = 'red',
             color = 'black') +
  labs(title = "FC Bayern Munich vs Bundesliga Clubs",
       subtitle = "Season 2020-21\nData Source: FBREF") +
  theme_ggtern() +
  theme_arrowlarge() +
  theme(plot.title = element_text(size = 16, face = 'bold', color = 'black'),
        plot.subtitle = element_text(size = 12, face = 'italic', color = 'black'),
        legend.text = element_text(size = 10))

# 4.2) Comparing a player in his club ----

# Jamal Musiala from FC Bayern
musiala_tbl <- stats_2020_21_tbl %>%
  filter(player == 'Jamal Musiala') %>%
  rename(Passing = pass_pct,
         Dribbling = dribbling_pct,
         Shooting = shot_pct,
         League = comp)

stats_2020_21_tbl %>%
  filter(squad == 'Bayern Munich' & pos != 'GK') %>%
  rename(Passing = pass_pct,
         Dribbling = dribbling_pct,
         Shooting = shot_pct,
         League = comp) %>%
  ggtern(aes(x = Passing, y = Dribbling, z = Shooting)) +
  geom_point(size = 3,
             shape = 21,
             color = 'grey',
             fill = 'grey') +
  geom_point(data = musiala_tbl, aes(x = Passing, y = Dribbling, z = Shooting),
             size = 3,
             shape = 21,
             color = 'black',
             fill = 'red') +
  labs(title = "FC Bayern Munich:\nJamal Musiala's Overall Performance",
       subtitle = "Season 2020-21\nData Source: FBREF") +
    theme_ggtern() +
    theme_arrowlarge() +
    theme(plot.title = element_text(size = 16, face = 'bold', color = 'black'),
          plot.subtitle = element_text(size = 12, face = 'italic', color = 'black'))

# 4.3) Match-up between 2 clubs ----

manchester_tbl <- stats_2020_21_tbl %>%
  filter(squad %in% c('Manchester Utd', 'Manchester City') & pos != 'GK') %>%
  rename(Passing = pass_pct,
         Dribbling = dribbling_pct,
         Shooting = shot_pct,
         League = comp)

ggtern(manchester_tbl, aes(x = Passing, y = Dribbling, z = Shooting, fill = squad)) +
    geom_point(size = 3,
               shape = 21,
               color = 'black') +
    labs(title = "Manchester United vs Manchester City",
         subtitle = "Season 2020-21\nData Source: FBREF",
         fill = "English Premier League Clubs") +
    scale_fill_manual(values = c('light blue', 'red')) +
    theme_ggtern() + 
    theme_arrowlarge() +
    theme(plot.title = element_text(size = 16, face = 'bold', color = 'black'),
          plot.subtitle = element_text(size = 12, face = 'italic', color = 'black'),
          legend.text = element_text(size = 10))



# 4.4) Club's Squad Performance ----

# Analyze Tottenham Squad
tottenham_tbl <- stats_2020_21_tbl %>%
  filter(squad == 'Tottenham' & pos != 'GK') %>%
  rename(Passing = pass_pct,
         Dribbling = dribbling_pct,
         Shooting = shot_pct,
         Position = pos,
         Age = age,
         Player = player)

ggtern(tottenham_tbl, aes(x = Passing, y = Dribbling, z = Shooting, fill = squad)) +
  geom_point(size = 3,
             shape = 21,
             fill = 'blue',
             color = 'blue') +
  labs(title = "Tottenham Hotspurs Squad's Overall Performance",
       subtitle = "Season 2020-21\nData Source: FBREF") +
  theme_ggtern() + 
  theme_arrowlarge() +
  theme(plot.title = element_text(size = 16, face = 'bold', color = 'black'),
        plot.subtitle = element_text(size = 12, face = 'italic', color = 'black'),
        legend.text = element_text(size = 10))

# Interactive Plot

# Axes formatting
axis <- function(txt) {
  list(
    title = txt, tickformat = ".0%", tickfont = list(size = 10)
  )
}

ternaryAxes <- list(
  aaxis = axis("Passing"), 
  baxis = axis("Dribbling"), 
  caxis = axis("Shooting")
)

# Plot Title
label <- function(txt) {
  list(
    text = txt, 
    x = 0.1, y = 1,
    ax = 0, ay = 0,
    xref = "paper", yref = "paper", 
    align = "center",
    font = list(family = "arial", size = 30, face = "bold", color = "black")
  )
}

# Interactive Plot
tottenham_plty <- plot_ly(tottenham_tbl,
                          a = ~ Passing,
                          b = ~ Dribbling,
                          c = ~ Shooting,
                          color = I('blue'),
                          type = 'scatterternary',
                          mode = 'markers',
                          hoverinfo = 'text',
                          marker = list(size = 8),
                          text = ~paste0('</br> Player Name: ', Player,
                                         '</br> Age: ', Age,
                                         '</br> Position: ', Position)) %>%
  layout(ternary = ternaryAxes)

tottenham_plty
