library(sf)

data.seq = read.csv("www/Dev_TPMS.csv", stringsAsFactors = FALSE)
FBID = data.seq$V1

Symbol = data.seq$symbol
data.seq = data.seq[-6]
data_sets <- c("FBID", "Symbol")

shape <- read_sf(dsn = "www/germPoly/.", layer = "germPoly")
data.seq$TKVbin1 = cut(as.numeric(data.seq$TKV), breaks = c(0,10,100,250,1000,2500,100000), 
                       labels=c("NA","VeryLow","Low","Med","High","VeryHigh"))

data.seq$Bambin1 = cut(as.numeric(data.seq$Bam), breaks = c(0,10,100,250,1000,2500,100000), 
                       labels=c("NA","VeryLow","Low","Med","High","VeryHigh"))

data.seq$Cystbin1 = cut(as.numeric(data.seq$bamhs.bam1), breaks = c(0,10,100,250,1000,2500,100000), 
                        labels=c("NA","VeryLow","Low","Med","High","VeryHigh"))

data.seq$Virginbin1 = cut(as.numeric(data.seq$VirginNG4), breaks = c(0,10,100,250,1000,2500,100000), 
                          labels=c("NA","VeryLow","Low","Med","High","VeryHigh"))

# shape$FID_ = seq(from = 1, to = 340, by = 10)
shape$FID_[1] = "NR"
shape$FID_[shape$LineWt == 35] = "NA"
shape$FID_[shape$LineWt == 106] = "NA"
shape$FID_[shape$Color == 7] = "Black"
shape$FID_[shape$FID_ == 0] = "NA"
# shape$FID_[2] = "Med"
shape$FID_ = factor(shape$FID_, c("NR", "NA", "VeryLow", "Low", "Med", "High", "VeryHigh", "Black"))

saveRDS(data.seq, "preprocessed_seq_data.RDS", compress = TRUE)
saveRDS(shape, "preprocessed_sf.RDS", compress = TRUE)
