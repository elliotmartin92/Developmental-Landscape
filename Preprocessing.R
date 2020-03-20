library(sf)

data.seq = read.csv("www/Dev_TPMS.csv", stringsAsFactors = FALSE)
FBID = data.seq$V1

Symbol = data.seq$symbol
data.seq = data.seq[-6]
data_sets <- c("FBID", "Symbol")

shape <- read_sf(dsn = "www/germPoly/.", layer = "germPoly")
data.seq$TKVbin1 = cut(as.numeric(data.seq$TKV), breaks = c(-1,10,100,250,1000,2500,100000), 
                       labels=c("None","Very Low","Low","Med","High","Very High"))

data.seq$Bambin1 = cut(as.numeric(data.seq$Bam), breaks = c(-1,10,100,250,1000,2500,100000), 
                       labels=c("None","Very Low","Low","Med","High","Very High"))

data.seq$Cystbin1 = cut(as.numeric(data.seq$bamhs.bam1), breaks = c(-1,10,100,250,1000,2500,100000), 
                        labels=c("None","Very Low","Low","Med","High","Very High"))

data.seq$Virginbin1 = cut(as.numeric(data.seq$VirginNG4), breaks = c(-1,10,100,250,1000,2500,100000), 
                          labels=c("None","Very Low","Low","Med","High","Very High"))

# shape$FID_ = seq(from = 1, to = 340, by = 10)
shape$FID_[1] = "NR"
shape$FID_[shape$LineWt == 35] = "NA"
shape$FID_[shape$LineWt == 106] = "NA"
shape$FID_[shape$Color == 7] = "Black"
shape$FID_[shape$FID_ == 0] = "NA"
# shape$FID_[2] = "Med"
shape$FID_ = factor(shape$FID_, c("None", "Very Low", "Low", "Med", "High", "Very High", "Black"))
shape.plot = data.frame(shape)

saveRDS(data.seq, "preprocessed_seq_data.RDS", compress = TRUE)
saveRDS(shape.plot, "preprocessed_sf.RDS", compress = TRUE)
