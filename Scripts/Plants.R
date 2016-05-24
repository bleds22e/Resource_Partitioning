# Most Abundant Plants since 2010

# library
library(dplyr)
library(tidyr)

# read file
plants2010 <- read.csv("Portal_plant_1981_2015.csv")

plant_data <- select(plants2010, year, season, species, abundance) %>% 
              filter(year >= 2010)
spp <- select(plant_data, species) %>% distinct(species) %>% arrange(species)
spp

write.csv(spp, file = "Common_spp_2010.csv", row.names = FALSE)

plant_order <- select(plant_data, year, season, species, abundance) %>% 
               group_by(year, season, species) %>% 
               summarize(total_abund = sum(abundance)) %>% 
               arrange(year, season, desc(total_abund)) %>% 
               top_n(25)
spp_25 <- ungroup(plant_order) %>% select(species) %>% distinct(species) %>% arrange(species)
write.csv(spp_25, file = "Top_25_spp_season.csv", row.names = FALSE)

total_species <- read.csv("Portal_plant_species.csv", header = TRUE)
head(total_species)

colnames(spp) <- c("Sp.Code")

spp_full <- semi_join(total_species, spp, by = "Sp.Code")
spp_data <- select(spp_full, Sp.Code, Genus, Species, Common.Name, Family)
write.csv(spp_data, file = "Top_25_spp_season.csv", row.names = FALSE)

### most winter species collected

winter_spp <- plants2010 %>% select(year, season, species) %>% 
              filter(season == 'winter') %>% 
              group_by(year, species) %>% 
              summarise(spp_count = n())
winter_spp_count <- select(winter_spp, year, species) %>% 
                    group_by(year) %>% 
                    summarise(spp_total = n())

# 453.592 grams/lb

75 * 35
75 *30
100 *30
3500/453.592

# order 7 lbs of silica beads

7 *.8
7 * .2

# get 2 lbs of orange indicating beads and 5 lbs of white nonindicating beads

# all species counted in winter ever
plants <- read.csv("Portal_plant_1981_2015.csv")
winter_plants <- plants %>% select(season, species) %>% 
                 filter(season == 'winter') %>% 
                 distinct(species)

colnames(winter_plants) <- c("Season", "Sp.Code")
winter_sp <- left_join(winter_plants, total_species, by = "Sp.Code")
head(winter_sp)

winter_sp <- unite(winter_sp, FullName, Genus, Species, sep = " ")
winter_sp <- winter_sp %>% arrange(FullName)

write.csv(winter_sp, "Winter_sp.csv")
