# read FNA files
# EKB
# 2/9/2017

# to run this script, you will need to download bioconductor first:
# source("https://bioconductor.org/biocLite.R")
# biocLite("Biostrings")

#===== LIBRARIES =====#

library(Biostrings)

#===== READ IN FILES =====#

its <- readDNAStringSet("C:/Users/ellen.bledsoe/Dropbox/Portal/PORTAL_primary_data/DNA/Results_Jonah/Plants/ITS2/merged.prtrim.upf.filt.derep.mc2.repset.fna")
trnL <- readDNAStringSet("C:/Users/ellen.bledsoe/Dropbox/Portal/PORTAL_primary_data/DNA/Results_Jonah/Plants/reference_samples/ref_seq_otus.fna")

# ===== MAKE DATAFRAMES =====#
OTU_its <- names(its)
sequence_its <- paste(its)
df_its <- data.frame(OTU_its, sequence_its)

OTU_trnL <- names(trnL)
sequence_trnL <- paste(trnL)
df_trnL <- data.frame(OTU_trnL, sequence_trnL)

#===== WRITE CSV FILES =====#

write.csv(df_its, "data/its_otus.csv")
write.csv(df_trnL, "data/trnL_otus.csv")




