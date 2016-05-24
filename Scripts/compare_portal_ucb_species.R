# Compare Portal species list (as of Spring 2016) to Craine/Fierer DNA list
# Ellen Bledsoe
# 5/24/2016

library(dplyr)

##############################
# READ IN FILES

ucb_species <- read.csv("Data/DNA_library_FiererCraine.csv")
portal_species <- read.csv("Data/Portal_plant_species.csv")
example <- read.csv("Data/Example.csv")

##############################
# FUNCTIONS

for(this_level in c('d','k','p','c','o','f','g','s')){
  
  step_one=sapply(strsplit(as.character(ucb_species$Taxonomy), paste0(this_level,':')), '[', 2)
  step_two=sapply(strsplit(step_one, ','), '[', 1)
  ucb_species[,this_level]=step_two
  
}

##############################
# WRANGLE

