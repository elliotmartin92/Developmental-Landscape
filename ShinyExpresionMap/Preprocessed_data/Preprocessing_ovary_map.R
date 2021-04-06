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

saveRDS(shape.plot, "preprocessed_sf.RDS", compress = TRUE)
saveRDS(shape, "Preprocessed_data/preloaded_shape.RDS", compress = TRUE)
