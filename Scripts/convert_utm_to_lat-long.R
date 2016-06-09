# Convert UTM to Lat/Long
# EKB
# June 9, 2016

library(rgdal)
library(dplyr)

dna_collected <- read.csv("Data/specimen_collection.csv")


# prepare UTM coordinates matrix
utmcoor<-SpatialPoints(cbind(dna_collected$easting,dna_collected$northing), proj4string=CRS("+proj=utm +zone=12"))
#utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
#zone= UTM zone

# converting
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))


# attach to dna_collection
lat_long <- as.data.frame(longlatcoor)
as.data.frame(dna_collected)

long_lat_added <- bind_cols(dna_collected, lat_long)

names(long_lat_added)[15] <- "long"
names(long_lat_added)[16] <- "lat"

write.csv(long_lat_added, "Data/specimen_collection.csv")
