library(magick)
library(ggplot2)
library(patchwork)
library(cowplot)
library(multipanelfigure)

image_panel = function(path, colors_to_return, genotype_annotation,
                       red_annotation=NA, green_annotation=NA, blue_annotation=NA, label_letters) {
  # options(repr.plot.width = (2.0694), repr.plot.height = (1.1837))
  greyscale_annotate = function(greyscale, color_annotation, label_letters){
    grey_annotated = image_annotate(greyscale, text = color_annotation, gravity = "northeast", 
                                    color = "white", size = (12*3.402778), font = "Helvetica")
    grey_annotated_letter = image_annotate(grey_annotated, text = label_letters, gravity = "northwest", 
                                           location = "+5", color = "white", size = (12*3.402778), font = "Helvetica")
    image_write(grey_annotated_letter, path = "temp_ano.tif", format = "tiff")
    image_gg = "temp_ano.tif"
    gg = ggdraw() + draw_image(image_gg, scale = 1)+
      theme(plot.margin = unit(c(0, 0, 0, 0), units = "in"), aspect.ratio = (1.1837)/(2.0694))
    rm("temp_ano.tif")
    return(gg)
  }
  
  rgb_image_raw = image_read(path)
  rgb_greyscales = as.list(image_separate(rgb_image_raw))
  default_color_order = c("red", "green", "blue")
  default_color_order_pared = default_color_order[default_color_order %in% colors_to_return]
  greyscales_ordered = rgb_greyscales[order(factor(default_color_order_pared, levels = colors_to_return))]
  all_grey_annotation = c(red_annotation, green_annotation, blue_annotation)
  all_grey_annotation_ordered = all_grey_annotation[order(factor(default_color_order, levels = colors_to_return))]
  all_grey_annotation_pared = all_grey_annotation_ordered[!is.na(all_grey_annotation_ordered)]
  label_letters_grey = label_letters[-1]
  all_gg = mapply(FUN = greyscale_annotate, 
                  greyscale = greyscales_ordered, 
                  color_annotation = all_grey_annotation_pared, 
                  label_letters = label_letters_grey,
                  SIMPLIFY = FALSE, USE.NAMES = T)
  
  annotation_spacer = 0
  if ("red" %in% colors_to_return) {
    image_ano = image_annotate(rgb_image_raw, text = red_annotation, gravity = "northeast", 
                               color = "#ED1C24", size = 12*3.402778, font = "Helvetica",
                               location = paste0("+", annotation_spacer))
    annotation_spacer = annotation_spacer + strwidth(red_annotation , units = "inches", 
                                                     family = "Helvetica", ps = par(ps = 12*3.402778))*72
    
  }
  if ("green" %in% colors_to_return) {
    image_ano = image_annotate(image_ano, text = green_annotation, gravity = "northeast", 
                               color = "#00A651", size = 12*3.402778, font = "Helvetica",
                               location = paste0("+", annotation_spacer))
    annotation_spacer = annotation_spacer + strwidth(green_annotation , units = "inches", 
                                                     family = "Helvetica", ps = par(ps = 12*3.402778))*72
  }
  if ("blue" %in% colors_to_return) {
    image_ano = image_annotate(image_ano, text = blue_annotation, gravity = "northeast", 
                               color = "#00AEEF", size = 12*3.402778, font = "Helvetica",
                               location = paste0("+", annotation_spacer))
  }
  
  image_fully_ano = image_annotate(image_ano, text = genotype_annotation, gravity = "southwest", 
                                   color = "white", size = 12*3.402778, font = "Helvetica") %>% 
    image_annotate(text = label_letters[[1]], gravity = "northwest", color = "white", 
                   size = 12*3.402778, font = "Helvetica", location = "+5")
  
  image_write(image_fully_ano, path = "temp_image_ano.tif", format = "tiff")

  image_ano_gg = "temp_image_ano.tif"
  
  color_gg = ggdraw() + draw_image(image_ano_gg, scale = 1)+
    theme(plot.margin = unit(c(0, 0, 0, 0), units = "in"), aspect.ratio = (1.1837)/(2.0694))
  all_gg$color = color_gg
  image_order_vector = c("color", colors_to_return)
  all_gg_ordered = all_gg[order(factor(names(all_gg), levels = image_order_vector))]

  figure = multi_panel_figure(
    width = c(2.0694, 2.0694, 2.0694),
    height = c(1.1837), rows = 1, row_spacing = 0, column_spacing = 0.025, unit = "in")
  
  figure = figure %>% 
    fill_panel(all_gg_ordered[[1]], label = "", scaling = "none", panel_clip = "on") %>% 
    fill_panel(all_gg_ordered[[2]], label = "", scaling = "none", panel_clip = "on") %>% 
    fill_panel(all_gg_ordered[[3]], label = "", scaling = "none", panel_clip = "on")
  figure
  
  assembled_images = plot_grid(plotlist = all_gg_ordered, nrow = 1) #+theme(plot.margin = unit(c(0, 0, 0, 0), units = "in"))
  return(figure)
  
}