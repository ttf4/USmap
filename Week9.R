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

library(tidyverse)
library(choroplethrMaps)
library(cowplot)

# Load data

data("state.map")
states_50 <- state.map
states_48 <- map_data('state')

covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# Manipulate the covid data frame to include a "percent increase" variable

covid_increase <- covid %>%
  
  
# Join covid_increase the map data of the 48 or 50 states
  
map_data <- 
  
# Create yor map! Fill the color of each state according to its percent
# increase in cases.
  