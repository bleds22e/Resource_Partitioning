# Consolidate lists of vial numbers
# EKB
# 12/01/2016

########################
# LIBRARIES

library(tidyr)
library(dplyr)
library(stringr)

########################
# DATA

plants <- read.csv("~/Dropbox/Portal/PORTAL_primary_data/DNA/Plants/vial_id.csv", header = TRUE, stringsAsFactors = TRUE, colClasses = "character")
rodents <- read.csv("~/Dropbox/Portal/PORTAL_primary_data/DNA/Rodents/fecal_samples.csv", header = TRUE, colClasses = "character")

########################
# NEW DATASHEET

plant <- unite(data = plants, col = sample_id, id_prefix, plant_id, sep = "", remove = FALSE) %>% 
         unite(col = vial_id, vial_prefix, vial_barcode, sep = "", remove = FALSE) %>% 
         select(vial_id, sample_id)
plant$type = "plant"

rodent <- select(rodents, vial_id = vial_barcode, sample_id = PIT_tag)
rodent$type = "fecal"           
          
vial_id_df <- bind_rows(plant, rodent)
write.csv(vial_id_df, "~/Dropbox/Portal/PORTAL_primary_data/DNA/vial_id.csv")

########################
# WORK AREA
