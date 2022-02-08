### Plot of Top Goal Scorers of from African Cup of Nations 2022

## 1) Load Libraries ----

# Install ggtextures if it's not available
#devtools::install_github("clauswilke/ggtextures")

library(tidyverse)
library(countrycode)
library(ggimage)
library(ggflags)
library(ggtextures)
library(ggthemes)
library(magick)


## 2) Import Data ----

afcon_22_top_scorers_tbl <- read_csv('data/AFCON_22_Top_Scorers.csv')


## 3) Plot ----

# AFCON Top Scorer
afcon_22_top_scorers_tbl <- afcon_22_top_scorers_tbl %>%
  mutate(image = 'https://www.emoji.co.uk/files/phantom-open-emojis/activity-phantom/12606-soccer-ball.png') %>%
  # Add Country Code
  mutate(Country_Code = tolower(countrycode(National_Team,
                                            origin = 'country.name',
                                            destination = 'iso2c')))
# Plotting
afcon_22_top_scorers_tbl %>%
  ggplot(aes(x = reorder(Player_Name, Total_Goals), y = Total_Goals,
            image = image)) +
    # Add Football Emoji to the plot
    geom_isotype_col(img_width = grid::unit(1, 'native'), img_height = NULL,
                     ncol = NA, nrow = 1, hjust = 0, vjust = 0.5) +
    # Add Country Code Symbol
    geom_flag(y = -0.25, aes(country = Country_Code), size = 12) +
    coord_flip() +
    scale_y_continuous(breaks = seq(0,10,2),
                       expand = c(0.05,0),
                       limits = c(0,10)) +
  # Customize theme
  ggthemes::theme_fivethirtyeight() +
  labs(title = 'Top Scorers of the African Cup of Nation 2022',
       subtitle = 'Top Scorer: Vincent Aboubakar with 8 Goals',
       y = 'Number of Goals', x = NULL,
       caption = 'Data Source: FBREF') +
  theme(text = element_text(family = 'Arial'),
        plot.title = element_text(size = 22, face = 'bold'),
        plot.subtitle = element_text(size = 14, face = 'bold.italic'),
        plot.caption = element_text(face = 'italic'),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y =  element_blank())