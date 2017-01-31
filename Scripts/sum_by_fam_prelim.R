# make Jonah Inc data easier to manage
# Ellen Bledsoe
# Jan 2017

########################
# LIBRARIES

library(dplyr)
library(stringr)
library(ggplot2)

########################
# LOAD FILES

data <- read.csv("C:/Users/ellen.bledsoe/Dropbox/Portal/PORTAL_primary_data/DNA/Results_Jonah/Plants/JV22_closed_ref_trnl_otutable.csv", header = TRUE)

samples <- read.csv("C:/Users/ellen.bledsoe/Dropbox/Portal/PORTAL_primary_data/DNA/Rodents/fecal_samples.csv", header = TRUE)

########################
# CLEAN DATA

data <- data[-(c(452:453)),]

# taxonomy dataframe

taxa <- select(data, ï...OTU.ID, ConsensusLineage) %>% 
        rename(OTU.ID = ï...OTU.ID)

for(this_level in c('d','k','p','c','o','f','g','s')){
  # separate taxa into columns
  step_one=sapply(strsplit(as.character(taxa$ConsensusLineage), paste0(this_level,'__')), '[', 2)
  step_two=sapply(strsplit(step_one, ';'), '[', 1)
  taxa[,this_level]=step_two
}

# reads dataframe

reads <- data[,c(1,8:54)] %>% rename(OTU.ID = ï...OTU.ID)
reads <- tidyr::gather(reads, "Sample", "Proportion", 2:48) %>% 
         filter(Proportion != 0)

# samples dataframe

samples <- select(samples, vial_barcode:PIT_tag) %>% 
           rename(Sample = vial_barcode)

#########################
# SUMMARIZE DATA

### 'rank-abundance' for PPs

# All PPs
PP_all <- samples %>% filter(species == 'PP')

PP_reads <- inner_join(PP_all, reads, by = "Sample")
mean_sd <- PP_reads %>% group_by(OTU.ID) %>% 
           summarise_at(vars(Proportion), funs(mean, sd)) %>% 
           filter(mean >= 0.001) %>% 
           arrange(desc(mean))

ggplot(data = mean_sd) +
  geom_col(aes(x = reorder(OTU.ID, desc(mean)), y = mean)) +
  geom_errorbar(aes(x = OTU.ID, ymin = mean-sd, ymax = mean+sd)) +
  labs(x = "OTU.ID", y = "Mean Proportion", title = "All_PP") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1))

# Fresh and trap  
mean_sd_by_type <- PP_reads %>% group_by(OTU.ID, sample_type) %>% 
  summarise_at(vars(Proportion), funs(mean, sd)) %>% 
  filter(mean >= 0.005) %>% 
  arrange(desc(mean))

ggplot(data = mean_sd_by_type, aes(x = reorder(OTU.ID, desc(mean)), y = mean, fill = sample_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(x = OTU.ID, ymin = mean-sd, ymax = mean+sd), position = position_dodge()) +
  labs(x = "OTU.ID", y = "Mean Proportion", title = "PP by Sample Type") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1))

### sum by plant family

family <- select(taxa, OTU.ID, o) %>% group_by(o)
otu_reads <- select(reads, OTU.ID, Proportion) %>% 
             group_by(OTU.ID)
family_sum <- inner_join(family, otu_reads, by = "OTU.ID") %>% 
              summarise(sum = sum(Proportion)) %>% 
              arrange(desc(sum)) %>% 
              rename(family = o) %>% 
              filter(sum > 0.001)

ggplot(data = family_sum, aes(x = reorder(family, desc(sum)), y = sum)) +
  geom_col() +
  labs(x = "Plant Family", y = "Total Proportion", title = "Proportion of Reads by Family (> 0.001)") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1))

### trap vs fresh between individuals

