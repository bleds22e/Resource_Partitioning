install.packages("ShortRead")
install.packages("Biostrings")
install.packages("Bioconductir")
install.packages("seqinr")
library(seqinr)

its <- Biostrings::readDNAStringSet("bleds22e/Dropbox/Portal/PORTAL_primary_data/DNA/Results_Jonah/Plants/ITS2/merged.prtrim.upf.filt.derep.mc2.repset.fna")
head(its)

OTU <- names(its)
sequence <- paste(its)
df <- data.frame(OTU, sequence)
