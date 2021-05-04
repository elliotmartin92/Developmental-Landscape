library(sf)
library(tidyverse)

lines = read_sf(dsn = "germPoly", layer = "germ_soma_cartoon_lines_v2")
shape = read_sf(dsn = "germPoly", layer = "germ_soma_cartoon_poly_v2")
lines$cell_type = "line"
lines$region = "line"
lines$color = "line"
shape$color = "White"
line.plot = data.frame(lines) %>% select(-path)
shape.plot = data.frame(shape)
merge_plot = rbind(shape.plot, line.plot)
merge_plot$color = factor(merge_plot$color, 
                          c("line", "None", "Very Low", "Low", "Med", "High", "Very High", "Black", "White"))

dev_order = c("GSC", "CB", "2CC", "4CC", "8CC",
              "16CC_2A1", "16CC_2A2", "16CC_2AB", "16CC_2B", "16CC_3", "ST2", 
              "CC", "cap_cells", "pre-stalk", "EC_a", "EC_c", "EC_p", "polar", "FSC", "stalk", "background", "line")

shape = shape %>% arrange(match(cell_type, dev_order))

merge_plot =
merge_plot %>% 
  group_by(cell_type) %>% 
  mutate(region = factor(region, c("line", "germline", "soma", "background"))) %>% 
  ungroup() %>% 
  arrange(match(cell_type, dev_order)) %>% 
  mutate(cell_type = factor(cell_type, unique(cell_type)))

saveRDS(merge_plot, "ShinyExpresionMap/Preprocessed_data/preprocessed_sf.RDS", compress = TRUE)
saveRDS(shape, "ShinyExpresionMap/Preprocessed_data/preloaded_shape.RDS", compress = TRUE)
