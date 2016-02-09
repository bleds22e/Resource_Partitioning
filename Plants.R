# Most Abundant Plants since 2010

# library
library(dplyr)

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
