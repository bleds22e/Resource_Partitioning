# Prep ITS2 data from Jonah Inc.
# Ellen Bledsoe
# Jan 2017

########################
# LIBRARIES

library(dplyr)
library(ggplot2)

########################
# LOAD FILES

blast <- read.csv("~bleds22e/Documents/Git/Metagenomics/Plants/ITS_blast.csv", header = TRUE, na.strings = "")
no_blast <- read.csv("~bleds22e/Documents/Git/Metagenomics/Plants/ITS_no_blast.csv", header = TRUE, na.strings = "")
plants <- read.csv("~bleds22e/Documents/Git/Metagenomics/CollectionData/plant_voucher_collection.csv", header = TRUE)

########################
# CLEAN DATA

# Blast
blast <- select(blast, -Sum)
blast <- blast[-c(463:464),]
reads_b <- tidyr::gather(blast, "Sample", "Reads", S008809.Wisely:S009099.Wisely) %>%
           filter(Reads != 0)
reads_b <- tidyr::separate(reads_b, Sample, into = c("Sample", "Wisely")) %>%
           select(-Wisely)

# No Blast
no_blast <- select(no_blast, -Sum)
reads_nb <- tidyr::gather(no_blast, "Sample", "Reads", S008809.Wisely:S009099.Wisely) %>%
  filter(Reads != 0)
reads_nb <- tidyr::separate(reads_nb, Sample, into = c("Sample", "Wisely")) %>%
  select(-Wisely)

# vouchers dataframe

vouchers <- select(plants, vial_barcode, label_number) %>% 
            rename(Sample = vial_barcode)
vouchers_b <- semi_join(reads_b, vouchers, by = "Sample")
vouchers_nb <- semi_join(reads_nb, vouchers, by = "Sample")

# fecal sample dataframe

fecal_b <- anti_join(reads_b, vouchers, by = "Sample")
fecal_nb <- anti_join(reads_nb, vouchers, by = "Sample")

# taxa dataframe (from Blast)

for(this_level in c('k','p','c','o','f','g','s')){
  # separate taxa into columns
  step_one=sapply(strsplit(as.character(vouchers_b$ConsensusLineage), paste0(this_level,'__')), '[', 2)
  step_two=sapply(strsplit(step_one, ';'), '[', 1)
  vouchers_b[,this_level]=step_two
}

taxa <- select(vouchers_b, OTU.ID, k:s) %>% group_by(OTU.ID) %>% distinct(OTU.ID, .keep_all = TRUE)

########################
# UNIQUE OTUs for VOUCHERS

match_b <- select(vouchers_b, OTU.ID, Sample, Reads) %>% group_by(Sample) %>% filter(Reads == max(Reads))
match_nb <- select(vouchers_nb, OTU.ID, Sample, Reads) %>% group_by(Sample) %>% filter(Reads == max(Reads))

length(unique(match_b$OTU.ID))

###########################
# MATCH OTU w/ BLAST TAXA

blast_taxa <- semi_join(taxa, match_b, by = "OTU.ID")
blast_taxa <- blast_taxa %>% group_by(OTU.ID) %>% summarise(count = n())

###########################
# WORK AREA

