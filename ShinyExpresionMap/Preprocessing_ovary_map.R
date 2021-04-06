library(sf)
library(org.Dm.eg.db)
library(annotate)
library(tidyverse)

fbgn_to_symbol =  function(fbid){
  AnnotationDbi::select(org.Dm.eg.db, fbid, 
                        columns=c("SYMBOL"), 
                        keytype="FLYBASE")
}

data.seq = readRDS("TPMs/Mean_TPMs_and_text.RDS")
data.seq$FBGN = as.character(data.seq$FBGN)
Symbol = fbgn_to_symbol(data.seq$FBGN)[[2]]
data.seq$symbol = Symbol

# data.seq = data.seq[-6]
data.seq$TKVbin1 = cut(as.numeric(data.seq$MeanTPM_TKV_input), breaks = c(-1,10,100,250,1000,2500,100000), 
                       labels=c("None","Very Low","Low","Med","High","Very High"))

data.seq$Bambin1 = cut(as.numeric(data.seq$MeanTPM_BamRNAi_input), breaks = c(-1,10,100,250,1000,2500,100000), 
                       labels=c("None","Very Low","Low","Med","High","Very High"))

data.seq$Cystbin1 = cut(as.numeric(data.seq$MeanTPM_BamHSbam_input), breaks = c(-1,10,100,250,1000,2500,100000), 
                        labels=c("None","Very Low","Low","Med","High","Very High"))

data.seq$Virginbin1 = cut(as.numeric(data.seq$MeanTPM_youngWT_input), breaks = c(-1,10,100,250,1000,2500,100000), 
                          labels=c("None","Very Low","Low","Med","High","Very High"))

data.seq$Adult = cut(as.numeric(data.seq$MeanTPM_pelo_cyo_input), breaks = c(-1,10,100,250,1000,2500,100000), 
                          labels=c("None","Very Low","Low","Med","High","Very High"))


shape = read_sf(dsn = "ShinyExpresionMap/www/germPoly", layer = "germPoly")
# shape$FID_ = seq(from = 1, to = 340, by = 10)
shape$FID_[1] = "NR"
shape$FID_[shape$LineWt == 35] = "NA"
shape$FID_[shape$LineWt == 106] = "NA"
shape$FID_[shape$Color == 7] = "Black"
shape$FID_[shape$FID_ == 0] = "NA"
# shape$FID_[2] = "Med"
shape$FID_ = factor(shape$FID_, c("None", "Very Low", "Low", "Med", "High", "Very High", "Black"))
shape.plot = data.frame(shape)

saveRDS(data.seq, "ShinyExpresionMap/Preprocessed_data/preprocessed_seq_data.RDS", compress = TRUE)
saveRDS(shape.plot, "ShinyExpresionMap/Preprocessed_data/preprocessed_sf.RDS", compress = TRUE)
saveRDS(shape, "ShinyExpresionMap/Preprocessed_data/preloaded_shape.RDS", compress = TRUE)
