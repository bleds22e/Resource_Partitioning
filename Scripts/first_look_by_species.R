# Preliminary genetic data

library(tidyr)
library(dplyr)

# files

data <- read.csv("/Users/bleds22e/Dropbox/Portal/PORTAL_primary_data/DNA/Results_Jonah/Plants/JV22_closed_ref_trnl_otutable.csv", header = TRUE)
data <- data[1:4,-(2:7)]
data <- tidyr::gather(data, vial_barcode, proportion, -X.OTU.ID)
data <- tidyr::spread(data, X.OTU.ID, proportion)

samples <- read.csv("/Users/bleds22e/Dropbox/Portal/PORTAL_primary_data/DNA/Rodents/fecal_samples.csv", header = TRUE)
samples <- select(samples, vial_barcode, species, sex, sample_type)

# summarize

dna <- inner_join(data, samples, by = "vial_barcode")
summary <- dna %>% group_by(species) %>% 
       summarise_at(.cols = c("OTU1199", "OTU150", "OTU20237", "OTU956"), .funs = funs(mean))
fresh <- dna %>% filter(sample_type == "fresh") %>% 
          group_by(species) %>% 
          summarise_at(.cols = c("OTU1199", "OTU150", "OTU20237", "OTU956"), .funs = funs(mean))
          