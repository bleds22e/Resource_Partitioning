# make Jonah Inc data easier to manage
# Ellen Bledsoe
# Jan 2017

########################
# LIBRARIES

library(dplyr)
library(stringr)

########################
# LOAD FILES

data <- read.csv("~bleds22e/Dropbox/Portal/PORTAL_primary_data/DNA/Results_Jonah/Plants/JV22_closed_ref_trnl_otutable.csv", header = TRUE)

samples <- 

########################
# CLEAN DATA

data <- data[-(c(452:453)),]

# taxonomy dataframe

taxa <- select(data, X.OTU.ID, ConsensusLineage) %>% 
        rename(OTU.ID = X.OTU.ID)

for(this_level in c('d','k','p','c','o','f','g','s')){
  # separate taxa into columns
  step_one=sapply(strsplit(as.character(taxa$ConsensusLineage), paste0(this_level,'__')), '[', 2)
  step_two=sapply(strsplit(step_one, ';'), '[', 1)
  taxa[,this_level]=step_two
}

# sample and read dataframe

reads <- data[,c(1,8:54)] %>% rename(OTU.ID = X.OTU.ID)
reads <- tidyr::gather(reads, "Sample", "Proportion", 2:48) %>% 
         filter(Proportion != 0)

#########################
# SUMMARIZE DATA

