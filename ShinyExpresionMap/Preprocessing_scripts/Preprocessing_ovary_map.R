library(sf)
library(tidyverse)

lines = read_sf(dsn = "germPoly", layer = "germ_soma_cartoon_lines_simplified")
shape = read_sf(dsn = "germPoly", layer = "germ_soma_cartoon_shape_simplified_recover")
lines$cell_type = "line"
lines$region = "line"
lines$color = "line"
shape$color = "White"
line.plot = data.frame(lines)
shape.plot = data.frame(shape)
merge_plot = rbind(shape.plot, line.plot)
merge_plot$color = factor(merge_plot$color, 
                          c("line", "None", "Very Low", "Low", "Med", "High", "Very High", "Black", "White"))

merge_plot =
merge_plot %>% 
  group_by(cell_type) %>% 
  mutate(region = factor(region, c("line", "germline", "soma", "background"))) %>% 
  arrange(region) %>% 
  ungroup() %>% 
  mutate(cell_type = factor(cell_type, unique(cell_type)))

saveRDS(merge_plot, "ShinyExpresionMap/Preprocessed_data/preprocessed_sf.RDS", compress = TRUE)
saveRDS(shape, "ShinyExpresionMap/Preprocessed_data/preloaded_shape.RDS", compress = TRUE)
