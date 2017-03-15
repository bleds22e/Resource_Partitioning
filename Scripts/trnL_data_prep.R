# Prep trnL data from Jonah Inc.
# Ellen Bledsoe
# Jan 2017

########################
# LIBRARIES

library(dplyr)
library(stringr)
library(ggplot2)

########################
# LOAD FILES

trnL <- read.csv("./Plants/trnL_fecal_samples.csv", header = TRUE)
samples <- read.csv("./CollectionData/fecal_sample_collection.csv", header = TRUE)

########################
# CLEAN DATA

trnL <- trnL[-(c(452:453)),]

# taxonomy dataframe

taxa_trnL <- select(trnL, OTU.ID, ConsensusLineage)

for(this_level in c('d','k','p','c','o','f','g','s')){
  # separate taxa into columns
  step_one=sapply(strsplit(as.character(taxa_trnL$ConsensusLineage), paste0(this_level,'__')), '[', 2)
  step_two=sapply(strsplit(step_one, ';'), '[', 1)
  taxa_trnL[,this_level]=step_two
}

taxa_trnL <- rename(taxa_trnL, Family = o, Genus = g, Species = s)

# reads dataframe

reads <- trnL[,c(1,8:54)]
reads <- tidyr::gather(reads, "Sample", "Reads", 2:48) %>% 
  filter(Reads != 0)

# samples dataframe

samples <- select(samples, vial_barcode:PIT_tag) %>% 
  rename(Sample = vial_barcode)
