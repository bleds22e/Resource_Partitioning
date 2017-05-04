# NMDS of 2016 Diet Data
# April 2017

# LOAD LIBRARIES #

library(vegan)
library(dplyr)
library(ggplot2)

# LOAD DATA #

setwd("C:/Users/ellen.bledsoe/Desktop/Git/Metagenomics")

# trnL proportion data
trnL <- read.csv("./Plants/trnL_fecal_samples.csv", header = TRUE)
# get prepped ITS data
#     - called fecal
source('ITS2_data_prep.R')
# fecal sample collection data
groups <- read.csv("./CollectionData/fecal_sample_collection.csv", header = TRUE)

#==============================================
# DATA PREP # 

# fecal collection group dataframe
groups <- groups %>%                  # filter fecal samples by collection period and type
  filter(year == '2016', sample_type == 'trap') %>% 
  select(vial_barcode, plot, species)

groups$group = NA                     # create grouping column based on species and plot
for (i in 1:length(groups$species)) {
  if (groups$species[i] == 'DO') {
    groups$group[i] = 'DO'
  } else if (groups$species[i] == 'DM') {
    groups$group[i] = 'DM'
  } else if (groups$plot[i] %in% c(4, 11, 14, 17)) {
    groups$group[i] = 'PP_control'
  } else {
    groups$group[i] = 'PP_exclosure'
  }
}

# trnL dataframe
trnL <- trnL[-(452:453), -c(1:7)]     # remove unnecessary columns and summary rows
trnL <- subset(trnL, Sum > 0.01)      # filter to remove excessive zeros
trnL <- trnL[,-48]                    # remove Sum column used to filter
trnL <- as.data.frame(t(trnL)) %>%    # rotate data and create vial_barcorde column to filter
        tibble::rownames_to_column("vial_barcode")
trnL <- semi_join(trnL, groups, by = 'vial_barcode') %>% # filter to get only trap samples
        tibble::column_to_rownames("vial_barcode")
trnL <- as.data.frame(lapply(trnL, as.numeric))

# ITS dataframe
ITS <- tidyr::spread(fecal, Sample, Reads) %>% select(-DF)
ITS[is.na(ITS)] = 0

ITS_ID <- ITS[,1]                     # turn reads into proportions
ITS <- sweep(ITS[,-1], 2, colSums(ITS[,-1]), '/')
ITS <- cbind(ITS_ID, ITS) %>% rename(OTU.ID = ITS_ID)
ITS$Sum = rowSums(ITS[,-1])
ITS <- subset(ITS, Sum > 0.01) %>% select(-Sum)
ITS <- as.data.frame(t(ITS)) %>%      # rotate data and create vial_barcorde column to filter
       tibble::rownames_to_column("vial_barcode")
ITS <- semi_join(ITS, groups, by = 'vial_barcode') %>% # filter to get only trap samples
       tibble::column_to_rownames("vial_barcode")
ITS <- as.data.frame(lapply(ITS, as.numeric))

#===============================================
# RUN NMDS and PLOT RESULTS #

### trnL ###

# use vegan package to run NMDS
dist_trnL <- metaMDS(trnL, distance = "binomial", trymax = 50)

# make dataframe with with MDS values and grouping variable
NMDS <- data.frame(MDS1 = dist_trnL$points[,1], 
                   MDS2 = dist_trnL$points[,2], 
                   group = groups$group)

# get mean point for each group
NMDS.mean <- aggregate(NMDS[,1:2], list(group = groups$group), mean)

# save results of ordiellipse() as an object
plot(dist_trnL$points)
ord <- ordiellipse(dist_trnL, 
                   groups$group, 
                   display = "sites", 
                   kind = "se", 
                   conf = 0.95, 
                   label = T)

# dataframe containing values to show ellipses
df_ell <- data.frame()
for(g in levels(NMDS$group)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$group==g, ], 
            vegan:::veganCovEllipse(cov.wt(cbind(MDS1, MDS2), wt=rep(1/length(MDS1),
            length(MDS1)))$cov, center=c(mean(MDS1), mean(MDS2))))),
            group = g))
}

# use ggplot2 to plot the data
ggplot(data = NMDS, aes(MDS1, MDS2)) + 
  geom_point(aes(color = group)) +
  geom_path(data = df_ell, aes(x = MDS1, y = MDS2,colour = group), size = 1) +
  annotate("text", x = NMDS.mean$MDS1, y = NMDS.mean$MDS2, label = NMDS.mean$group) +
  theme_bw()

### ITS ###

# use vegan package to run NMDS
dist_ITS <- metaMDS(ITS, distance = "binomial", trymax = 50)

# make dataframe with with MDS values and grouping variable
NMDS <- data.frame(MDS1 = dist_ITS$points[,1], 
                   MDS2 = dist_ITS$points[,2], 
                   group = groups$group)

# get mean point for each group
NMDS.mean <- aggregate(NMDS[,1:2], list(group = groups$group), mean)

# save results of ordiellipse() as an object
plot(dist_ITS$points)
ord <- ordiellipse(dist_ITS, 
                   groups$group, 
                   display = "sites", 
                   kind = "se", 
                   conf = 0.95, 
                   label = T)

# dataframe containing values to show ellipses
df_ell <- data.frame()
for(g in levels(NMDS$group)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$group==g, ], 
            vegan:::veganCovEllipse(cov.wt(cbind(MDS1, MDS2), wt=rep(1/length(MDS1),
            length(MDS1)))$cov, center=c(mean(MDS1), mean(MDS2))))),
                                group = g))
}

# use ggplot2 to plot the data
ggplot(data = NMDS, aes(MDS1, MDS2)) + 
  geom_point(aes(color = group)) +
  geom_path(data = df_ell, aes(x = MDS1, y = MDS2,colour = group), size = 1) +
  annotate("text", x = NMDS.mean$MDS1, y = NMDS.mean$MDS2, label = NMDS.mean$group) +
  theme_bw()
