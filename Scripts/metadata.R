# Prep data for Jonah Ventures metadata
# EKB
# 12/5/2016

######################
# LIBRARY

library(dplyr)

######################
# DATA

plants <- read.csv("~/Dropbox/Portal/PORTAL_primary_data/DNA/Plants/plant_collection.csv", header = TRUE)
fecal <- read.csv("~/Dropbox/Portal/PORTAL_primary_data/DNA/Rodents/fecal_samples.csv", header = TRUE)
vial_id <- read.csv("~/Dropbox/Portal/PORTAL_primary_data/DNA/vial_id.csv", header = TRUE)

######################
# MERGE

colnames(vial_id) <- c("Sample#", "vial_id", "sample_id", "type")

# match plant data with vial id data
plants <- select(plants, month, day, year, sci_name_profID, label_number, lat, long)
colnames(plants) <- c("month", "day", "year", "sci_name", "sample_id", "lat", "long")

vial_id <- left_join(vial_id, plants, by = "sample_id")
