library(sf)
library(tidyverse)

lines = read_sf(dsn = "ShinyExpresionMap/www/germPoly", layer = "germ_soma_cartoon_lines")
shape = read_sf(dsn = "ShinyExpresionMap/www/germPoly", layer = "germa_soma_cartoon_shape_simplified")
lines$color = "Black"
shape$color = ""
# shape$FID_ = seq(from = 1, to = 340, by = 10)
# shape$FID_[1] = "NR"
# shape$FID_[shape$LineWt == 35] = "NA"
# shape$FID_[shape$LineWt == 106] = "NA"
# shape$FID_[shape$Color == 7] = "Black"
# shape$FID_[shape$FID_ == 0] = "NA"
# shape$FID_[2] = "Med"
shape$color = factor(shape$color, c("None", "Very Low", "Low", "Med", "High", "Very High", "Black"))
lines$region = "lines"
line.plot = data.frame(lines)
shape.plot = data.frame(shape)
lines.plot = data.frame(shape)
merge_plot = full_join(shape.plot, line.plot)


saveRDS(merge_plot, "ShinyExpresionMap/Preprocessed_data/preprocessed_sf.RDS", compress = TRUE)
saveRDS(shape, "ShinyExpresionMap/Preprocessed_data/preloaded_shape.RDS", compress = TRUE)
