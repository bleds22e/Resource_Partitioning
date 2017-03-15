# Prep ITS2 data from Jonah Inc.
# Ellen Bledsoe
# Jan 2017

########################
# LIBRARIES

library(dplyr)
library(ggplot2)

########################
# LOAD FILES

blast <- read.csv("./Plants/ITS_blast.csv", header = TRUE, na.strings = "")
no_blast <- read.csv("./Plants/ITS_no_blast.csv", header = TRUE, na.strings = "")
plants <- read.csv("./CollectionData/plant_voucher_collection.csv", header = TRUE)

########################
# CLEAN DATA

# Blast and no blast files

blast <- select(blast, -Sum)
blast <- blast[-c(463:464),]
no_blast <- select(no_blast, -Sum)

# Pull out ConsensusLineage

taxa_its <- select(blast, OTU.ID, ConsensusLineage)

for(this_level in c('k','p','c','o','f','g','s')){
  # separate taxa into columns
  step_one=sapply(strsplit(as.character(taxa_its$ConsensusLineage), paste0(this_level,'__')), '[', 2)
  step_two=sapply(strsplit(step_one, ';'), '[', 1)
  taxa_its[,this_level]=step_two
}

taxa_its <- select(taxa_its, OTU.ID, k:s) %>% distinct(OTU.ID, .keep_all = TRUE) %>% arrange(OTU.ID)
taxa_its <- rename(taxa_its, Family = f, Genus = g, Species = s)

# Restructure and combine

blast <- select(blast, -ConsensusLineage)
all_ITS <- bind_rows(blast, no_blast, .id = "DF")

all_ITS <- tidyr::gather(all_ITS, "Sample", "Reads", S008809.Wisely:S009099.Wisely) %>%
           filter(Reads != 0)
all_ITS <- tidyr::separate(all_ITS, Sample, into = c("Sample", "Wisely")) %>%
           select(-Wisely)

# vouchers dataframe

vouchers <- select(plants, vial_barcode, sci_name_profID) %>% 
            rename(Sample = vial_barcode)
vouchers_its <- right_join(all_ITS, vouchers, by = "Sample")

# fecal sample dataframe

fecal <- anti_join(all_ITS, vouchers, by = "Sample")

########################
# UNIQUE OTUs for VOUCHERS

best_match <- select(vouchers_its, OTU.ID, Sample, Reads, sci_name_profID) %>% group_by(Sample) %>% filter(Reads == max(Reads))
best_match <- filter(best_match, Reads >= 1000)

length(unique(best_match$OTU.ID))
count_OTU <- best_match %>% ungroup() %>% count(OTU.ID)


