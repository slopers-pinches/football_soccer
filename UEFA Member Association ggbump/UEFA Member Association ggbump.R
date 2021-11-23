### ggbump plot of UEFA Member Association Coefficienets ###

# Install packages
#devtools::install_github("rensa/ggflags") # for circular shape
#install.packages('cowplot')
#install.packages('magick')

# Load libraries
library(tidyverse)
library(ggbump)
library(countrycode)
library(ggflags)
library(RColorBrewer)

# Import data
uefa <- read_csv('uefa_member_association_coeff.csv')
glimpse(uefa)

## Transform & Pre-processing df ##
# Transform df to long format
uefa <- uefa %>%
            pivot_longer(cols = !UEFA_Country,
                         names_to = 'Season', values_to = 'UEFA_Coefficient')

# Preview
head(uefa, 5)

# Filter by Nordic Countries
countries <- c('Denmark', 'Norway', 'Sweden', 'Finland', 'Iceland')
uefa.5 <- uefa %>%
            filter(UEFA_Country %in% countries) %>%
            # Calculate the rank of Highest UEFA Coefficient by Season
            group_by(Season) %>%
            mutate(Rank = rank(desc(UEFA_Coefficient), ties.method = 'first')) %>%
            ungroup() %>%
            # Bin Season
            mutate(season_bin = case_when(Season == '2017–18' ~ 1,
                                          Season == '2018–19' ~ 2,
                                          Season == '2019–20' ~ 3,
                                          Season == '2020–21' ~ 4,
                                          Season == '2021–22' ~ 5)) %>%
            # Add Country Code
            mutate(Country_Code = tolower(countrycode(UEFA_Country,
                                    origin = 'country.name',
                                    destination = 'genc2c')))

# Import logo
logo.2 <- magick::image_read('logo/uefa_logo_transparent_ver2.png')

## Plot ##
p <- uefa.5 %>%
  ggplot(aes(x = season_bin, y = Rank, color = UEFA_Country)) +
    geom_point(size = 6) +
    geom_bump(size = 2, smooth = 8) +
    # Add flags
    geom_flag(data = uefa.5 %>% filter(season_bin == 1),
              aes(country = Country_Code), size = 10) +
    geom_flag(data = uefa.5 %>% filter(season_bin == 5),
              aes(country = Country_Code), size = 10) +
    scale_color_brewer(palette = 'Dark2', direction = 1) +
    # Rename x-axis
    scale_x_continuous(breaks = (1:5), labels = c('2017-18', '2018-19', '2019-20',
                                                  '2020-21', '2021-22')) +
    scale_y_reverse() +
    labs(x = 'Season',
         y = 'Ranking',
         title = 'UEFA Association Coefficienet Ranking Among Nordic Countries',
         subtitle = 'Season 2017-18 to 2021-22') +
    theme_minimal() +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          panel.background = element_rect(fill = 'gray90', color = 'transparent'),
          plot.background = element_rect(fill = 'gray90'),
          axis.text.x = element_text(size = 12, face = 'bold'),
          axis.text.y = element_text(size = 12, face = 'bold'),
          axis.text = element_text(size = 14, face = 'bold'),
          axis.title = element_text(size = 16, face = 'bold'),
          title = element_text(size = 20, face = 'bold'))

# Add logo to the top right-hand corner
cowplot::ggdraw() +
  cowplot::draw_plot(p) +
  cowplot::draw_image(logo.2, x = 1, y = 1, scale = 0.5,
                      hjust = 1, vjust = 1, halign = 1, valign = 1,
                      width = 0.15)
