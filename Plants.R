# Most Abundant Plants since 2010

# library
library(dplyr)

# read file
plants2010 <- read.csv("Portal_plant_1981_2015.csv")

plant_data <- select(plants2010, year, season, species, abundance) %>% 
              filter(year >= 2010)
spp <- select(plant_data, species) %>% distinct(species) %>% arrange(species)
spp


