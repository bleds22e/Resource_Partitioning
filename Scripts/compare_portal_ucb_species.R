# Compare Portal species list (as of Spring 2016) to Craine/Fierer DNA list
# Ellen Bledsoe
# 5/24/2016

library(dplyr)
library(stringr)

##############################
# READ IN FILES

ucb_species <- read.csv("Data/DNA_library_FiererCraine.csv")
portal_species <- read.csv("Data/Portal_plant_species.csv")
example <- read.csv("Data/Example.csv")

##############################
# CLEAN DATASETS

# UCB

# remove semicolon from end of string
ucb_species$Taxonomy <- str_replace_all(ucb_species$Taxonomy, ";", "")

# separate taxa into columns
for(this_level in c('d','k','p','c','o','f','g','s')){
  step_one=sapply(strsplit(as.character(ucb_species$Taxonomy), paste0(this_level,':')), '[', 2)
  step_two=sapply(strsplit(step_one, ','), '[', 1)
  ucb_species[,this_level]=step_two
}

# select only necessary columns, group and arrange
ucb_species <- select(ucb_species, Family = o, Genus = g, Genus_sp = s) %>%
               group_by(Family, Genus, Genus_sp) %>% 
               arrange()

# PORTAL

# include all possible genus variations with to ensure accurate number of matches
df <- data.frame(Genus = c("Aristida", "Hamulosa", "Dimorphocarpa", "Dithyrea",
                           "Machaeranthera", "Haplopappus", "Haplopappus", "Isocoma",
                           "Nuttallanthus", "Linaria", "Uropappus", "Microseris",
                           "Urochloa", "Panicum", "Phemeranthus", "Talinum",
                           "Talinum", "Phemeranthus"),
                 Species = c("ternipes", "ternipes", "wislizeni", "wislizeni",
                             "gracilis", "gracilis", "tenuisectus", "tenuisectus",
                             "texanus", "texanus", "lindleyi", "lindleyi",
                             "arizonicum", "arizonicum", "angustissimum", "angustissimum",
                             "aurantiacus", "aurantiacus"))
genus_species <- portal_species %>% select(Genus, Species)
portal_genus_sp <- bind_rows(genus_species, df)

# make a column of genus and species to make UCB
portal_g_s <- tidyr::unite(portal_genus_sp, Genus_sp, Genus, Species, sep = " ") %>% 
              select(Genus_sp)

##############################
# WRANGLE

genus_sp <- semi_join(ucb_species, portal_g_s, by = "Genus_sp")
genus_sp1 <- semi_join(ucb_species, portal_g_s, by = "Genus_sp") %>% distinct() %>% arrange()

not_found <- anti_join(portal_g_s, ucb_species, by = c("Genus", "Genus_sp"))

# percent plants in each family

portal_families <- select(portal_species, Family, Genus, Species) %>% 
                   filter(Genus != "Unknown", Species != "sp.", Species != "spp.") %>% 
                   group_by(Family) %>% 
                   arrange()
portal_families <- select(portal_families, Family) %>% 
                   count(Family) %>%
                   mutate(relative = n/sum(n))

write.csv(genus_sp1, file = "Data/UCB_portal_matches.csv")
write.csv(portal_families, file = "Data/Portal_families.csv")
