library(sf)
library(tidyverse)

lines = read_sf(dsn = "germPoly", layer = "germ_soma_cartoon_lines_v12")
shape = read_sf(dsn = "germPoly", layer = "germ_soma_cartoon_poly_v12")

preprocess_map = function(dev_order, sf_filename, shape_filename){
  filtered_lines = lines %>% 
    filter(cell_type %in% c(dev_order, NA)) 
  
  filtered_shape = shape %>% 
    filter(cell_type %in% dev_order)
  
  filtered_shape$color = "White"
  filtered_lines$cell_type = "line"
  filtered_lines$region = "line"
  filtered_lines$color = "line"
  line.plot = data.frame(filtered_lines)
  shape.plot = data.frame(filtered_shape)
  merge_plot = rbind(shape.plot, line.plot)
  merge_plot$color = factor(merge_plot$color, 
                            c("line", "None", "Very Low", "Low", "Med", "High", "Very High", "Black", "White"))
  
  filtered_shape_ordered = filtered_shape %>% 
    arrange(match(cell_type, dev_order))
  
  merge_plot =
  merge_plot %>% 
    group_by(cell_type) %>% 
    mutate(region = factor(region, c("marker", "line", "germline", "soma", "background"))) %>% 
    ungroup() %>% 
    arrange(match(cell_type, dev_order)) %>% 
    mutate(cell_type = factor(cell_type, unique(cell_type)))
  
  saveRDS(merge_plot, paste0("ShinyExpresionMap/Preprocessed_data/", sf_filename), compress = TRUE)
  saveRDS(filtered_shape_ordered, paste0("ShinyExpresionMap/Preprocessed_data/", shape_filename), compress = TRUE)
}


quant_render_order = c("GSC", "CB", "2CC", "4CC", "8CC",
                                  "16CC_2A1", "16CC_2A2", "16CC_2AB", "16CC_2B", "16CC_3", "ST2", 
                                  "TF/CC", "EC_a", "EC_c", "EC_p", "FSC", "pre-stalk", "stalk", "polar", "background", "line")

preprocess_map(quant_render_order, "preprocessed_sf.RDS", "preloaded_shape.RDS")

cartoon_render_order = c("1B1", "oocyte", "GSC", "CB", "2CC", "4CC", "8CC",
                         "16CC_2A1", "16CC_2A2", "16CC_2AB", "16CC_2B", "16CC_3", "ST2", 
                         "TF/CC", "EC_a", "EC_c", "EC_p", "FSC", "pre-stalk", "stalk", "polar", "background", "line")

preprocess_map(cartoon_render_order, "preprocessed_sf_cartoon.RDS", "preloaded_shape_cartoon.RDS")


