# Plotting functions for quick looks at data
# Feb 21, 2017

#########################
# LIBRARIES

library(dplyr)
library(ggplot2)

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

# by plot type (fresh only)

PP_fresh <- PP_all %>% filter(sample_type == 'fresh')

PP_fresh$plot_type <- PP_fresh$plot
for (i in 1:length(PP_fresh$plot_type)){
  if (PP_fresh$plot[i] %in% c(4,11,14,17)){
    PP_fresh$plot_type[i] = 'control'
  } else {
    PP_fresh$plot_type[i] = 'krat_exclosure'
  }
}

PP_reads2 <- inner_join(PP_fresh, reads, by = "Sample")

mean_sd_plot_type <- PP_reads2 %>% group_by(OTU.ID, plot_type) %>% 
  summarise_at(vars(Proportion), funs(mean, sd)) %>% 
  filter(mean >= 0.005) %>% 
  arrange(desc(mean))

ggplot(data = mean_sd_plot_type, aes(x = reorder(OTU.ID, desc(mean)), y = mean, fill = plot_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(x = OTU.ID, ymin = mean-sd, ymax = mean+sd), position = position_dodge()) +
  labs(x = "OTU.ID", y = "Mean Proportion", title = "PP by Plot Type (fresh only)") +
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

pit_tags <- samples %>% group_by(PIT_tag) %>% 
  summarise(num_samples = n_distinct(sample_type)) %>% 
  filter(num_samples == 2)
both_types <- semi_join(samples, pit_tags, by = "PIT_tag")
add_reads <- inner_join(both_types, reads, by = "Sample") %>% 
  tidyr::unite(col = individual, species, sex, PIT_tag, sep = "_") %>% 
  filter(Proportion > 0.01)

ggplot(data = add_reads, aes(x = reorder(OTU.ID, desc(Proportion)), y = Proportion, fill = sample_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ individual) +
  labs(x = "OTU.ID", y = "Proportion") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1))
