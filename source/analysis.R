library(ggplot2)
library(tidyverse)
library(plotly)
library(leaflet)

# Reads the data from the incarceration trends file
data_incar <- read.csv("../incarceration_trends.csv")

# Creates a data frame that summarizes the total prison population of white
# and black races between 1990 and 2016
prison_pop_year <- data_incar %>%
  filter(year >= "1990", year <= "2016") %>%
  group_by(year) %>%
  summarize(
    black_prison_pop = sum(black_prison_pop, na.rm = TRUE),
    white_prison_pop = sum(white_prison_pop, na.rm = TRUE)
  ) %>%
  rename(Black = black_prison_pop, White = white_prison_pop) %>%
  select(year, Black, White) %>%
  gather(key = Race, value = population, -year)

# Creates a data frame that summarizes the total jail population of
# individuals from the white and black races based on urbanicity
jail_pop_urbanicity <- data_incar %>%
  group_by(urbanicity) %>%
  summarize(
    black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
    white_jail_pop = sum(white_jail_pop, na.rm = TRUE)
  ) %>%
  rename(Black = black_jail_pop, White = white_jail_pop) %>%
  select(urbanicity, Black, White) %>%
  gather(key = Race, value = population, -urbanicity)

# Creates a data frame that summarizes the total prison population of white
# and black races within each state in every recorded year altogether
prison_pop_location <- data_incar %>%
  group_by(state) %>%
  mutate(state = tolower(state.name[match(state, state.abb)])) %>%
  summarize(
    black_prison_pop = sum(black_prison_pop, na.rm = TRUE),
    white_prison_pop = sum(white_prison_pop, na.rm = TRUE)
  )

# Creates a bar chart that compares the number of black and white people in
# jail and based on urbanicity
data_compare <- print(ggplot(jail_pop_urbanicity) +
  geom_col(
    mapping = aes(x = urbanicity, y = population, fill = Race)
  ) +
  labs(
    title = "Total Jailed Population Count for Whites and Blacks by Urbanicity",
    x = "Urbanicity",
    y = "Total Jailed Population"
  )
)

# Creates a grouped bar chart displaying the total prison population of black
# and white races throughout the years of 1990 to 2016
data_trends <- print(ggplot(prison_pop_year) +
  geom_col(
    mapping = aes(x = year, y = population, fill = Race), position = "dodge"
  ) +
  labs(
    title = "Total Prison Population by Race per Year",
    x = "Year",
    y = "Prison Population Count"
  )
)

# Defines a minimalist theme for the map
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Loads shapefile of US states and joins prison population location
# data frame to it
state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(prison_pop_location, by = "state")

# Creates a map displaying the total prison population of blacks through all
# the years combined in each state
data_map <- print(ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_prison_pop),
    color = "white",
    size = .1
  ) +
  labs(
    title = "Total Black Prison Population in Each State",
    fill = "Population"
  ) +
  coord_map() +
  scale_fill_continuous(low = "grey", high = "blue") +
  blank_theme
)
