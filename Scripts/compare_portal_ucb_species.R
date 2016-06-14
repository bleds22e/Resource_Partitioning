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

# make a column of genus and species to make UCB
portal_g_s <- tidyr::unite(portal_species, Genus_sp, Genus, Species, sep = " ") %>% 
              select(Genus_sp)
# add genus_species column to original species list
portal_species <- bind_cols(portal_species, portal_g_s)
portal_species <- arrange(portal_species, Family, Genus, Genus_sp)

##############################
# WRANGLE

genus_sp <- semi_join(ucb_species, portal_species, by = "Genus_sp")
genus_sp1 <- semi_join(ucb_species, portal_species, by = "Genus_sp") %>% distinct()

fam_genus_sp <- semi_join(ucb_species, portal_species, by = c("Family", "Genus", "Genus_sp"))
fam_genus_sp1 <- semi_join(ucb_species, portal_species, by = c("Family", "Genus", "Genus_sp")) %>% distinct()

not_found <- anti_join(portal_species, ucb_species, by = c("Genus", "Genus_sp"))

