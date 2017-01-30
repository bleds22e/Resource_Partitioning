# make Jonah Inc data easier to manage
# Ellen Bledsoe
# Jan 2017

########################
# LIBRARIES

library(dplyr)
library(stringr)

########################
# LOAD FILES

data <- read.csv("C:/Users/ellen.bledsoe/Dropbox/Portal/PORTAL_primary_data/DNA/Results_Jonah/Plants/JV22_closed_ref_trnl_otutable.csv", header = TRUE)

########################
# CLEAN DATA

# taxonomy dataframe

taxa <- select(data, ï...OTU.ID, ConsensusLineage) %>% 
        rename(OTU.ID = ï...OTU.ID)

for(this_level in c('d','k','p','c','o','f','g','s')){
  # separate taxa into columns
  step_one=sapply(strsplit(as.character(taxa$ConsensusLineage), paste0(this_level,'__')), '[', 2)
  step_two=sapply(strsplit(step_one, ';'), '[', 1)
  taxa[,this_level]=step_two
}

# sample and read dataframe

reads <- data[,c(1,8:54)] %>% rename(OTU.ID = ï...OTU.ID)

reads <- tidyr::gather(reads, "Sample", "Proportion", 2:48)

#########################
# SUMMARIZE DATA

