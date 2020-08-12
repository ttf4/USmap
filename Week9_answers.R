USA <- read.csv("https://raw.githubusercontent.com/YouGov-Data/covid-19-tracker/master/data/united-states.csv")
View(USA)

library(tidyverse)
library(cowplot)
library(choroplethrMaps)

data("state.map")
states_50 <- state.map
states_48 <- map_data('state')

covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

covid_increase <- covid %>%
  mutate(date = as.Date(date),
         state = tolower(state)) %>%
  filter(date == max(date) | date == "2020-07-01") %>%
  rename(region = state) %>%
  select(date, region, cases) %>%
  spread(key = date, value = cases) %>%
  mutate(increase = `2020-07-21` - `2020-07-01`,
         percent_increase = (increase/`2020-07-01`)*100,
         percent_increase_factor = case_when(percent_increase <= 25 ~ "25",
                                             percent_increase <= 50 ~ "50",
                                             percent_increase <= 75 ~ "75",
                                             percent_increase <= 100 ~ "100",
                                             percent_increase <= 125 ~ "125",
                                             TRUE ~ ">125"),
         percent_increase_factor = factor(percent_increase_factor, levels =
                                            c("25", "50", "75", "100", "125", ">125")))

map_data <- states_50 %>%
  left_join(covid_increase, by = "region")

ggplot(data = map_data) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = percent_increase_factor)) +
  coord_fixed(ratio = 1.3) +
  scale_fill_brewer(palette = "OrRd") +
  theme_nothing() +
  theme(legend.position = "right", plot.title = element_text()) +  
  labs(fill = "% increase") +
  ggtitle("Percent increase since July 1")
