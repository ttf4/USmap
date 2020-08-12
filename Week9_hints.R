################################################################################
###
### Data analysis in R: Week 9
###
### Sara Gottlieb-Cohen, Manager of Statistical Support Services
### Marx Library
### Yale University
###
################################################################################

## Research question: What is the percent increase in Covid-19 cases across states
## in the US since July 1st?

# Load packages
install.packages("cowplot")
install.packages("choroplethrMaps")

library(tidyverse)
library(choroplethrMaps)
library(cowplot)

install.packages("viridis") 
library("viridis") 

# Load data

data("state.map")
states_50 <- state.map
states_48 <- map_data('state')
usa <- map_data("usa")

covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# Manipulate the covid data frame to make the following changes:
# 1. Convert "date" to a date formate instead of a character string
# 2. Filter to only include observations from July 1st and yesterday
# 3. Rename "state" as "region" in order to join it with the map data
# 4. Calculate the percent increase in cases for each state. Hint:
# you will first need to use "spread" to do this, or pivot_wider
# 5. You can choose to leave the "percen increase" variable continuous,
# or make it a factor with discrete levels. If you make it a factor,
# it is up to you how you bin the values. Hint: use "case_when."

covid_increase <- covid %>%
  mutate(date = as.Date(date)) %>%
  filter(date == "2020-07-01" | date == max(date)) %>%
  mutate(state = tolower(state)) %>%
  rename(region = state) %>%
  select(region, date, cases) %>%
  pivot_wider(names_from = date, values_from = cases) %>%
  rename(start = "2020-07-01", end = "2020-07-21") %>%
  mutate(percent_increase = ((end - start)/start)*100) %>%
  mutate(increase_factor = case_when(percent_increase < 5 ~ 0,
                                     percent_increase <= 25 ~ 1,
                                     percent_increase <= 50 ~ 2,
                                     percent_increase <= 75 ~ 3,
                                     percent_increase <= 100 ~ 4,
                                      percent_increase > 100 ~ 5),
         increase_factor = factor(increase_factor))


# Join covid_increase the map data of the 48 or 50 states
  
map_data <- states_48 %>%
  inner_join(covid_increase, by = "region")

# Create yor map! Fill the color of each state according to its percent
# increase in cases.

usa_base <- ggplot(data = usa, aes(x = long, y = lat, group = group)) +
  coord_fixed(ratio = 1.3) +
  geom_polygon(color = "black", fill = "grey") 

usa_base +
  geom_polygon(data = map_data, aes(fill = increase_factor), color = "grey", size = .1) +
  geom_polygon(color = "black", fill = NA, size = .3) +
  scale_fill_viridis(discrete = TRUE, option = "C", name = "Scale", labels = c("+ <5%", "+ 5-25%", "+ 25-50%", "+ 50-75%", "+ 75-100%", "+ >100%")) +
  labs(title = "% increase in COVID-19 cases in July 2020") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

ggplot(map_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = increase_factor), color = "grey", size = .1) +
  coord_fixed(ratio = 1.3) +
  scale_fill_viridis(discrete = TRUE, option = "C", name = "Scale", labels = c("+ <5%", "+ 5-25%", "+ 25-50%", "+ 50-75%", "+ 75-100%", "+ >100%")) +
  labs(title = "% increase in COVID-19 cases in July 2020") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())
