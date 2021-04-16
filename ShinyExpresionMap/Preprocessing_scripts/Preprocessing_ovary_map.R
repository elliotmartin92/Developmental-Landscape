library(sf)
library(tidyverse)

shape = read_sf(dsn = "www/germPoly", layer = "germPoly")
# shape$FID_ = seq(from = 1, to = 340, by = 10)
shape$FID_[1] = "white"
shape$FID_[shape$LineWt == 35] = "grey50"
shape$FID_[shape$LineWt == 106] = "grey50"
shape$FID_[shape$Color == 7] = "Black"
shape$FID_[shape$FID_ == 0] = "grey50"
# shape$FID_[2] = "Med"
shape.plot = data.frame(shape)

saveRDS(shape.plot, "Preprocessed_data/preprocessed_sf.RDS", compress = TRUE)
saveRDS(shape, "Preprocessed_data/preloaded_shape.RDS", compress = TRUE)
