library(magick)
library(ggplot2)
library(cowplot)
library(multipanelfigure)
library(extrafont)
library(ijtiff)
library(reticulate)
library(tools)
library(ijtiff)
source_python("../Paper/Helper_functions/pixel_size.py")

# font_import(paths = c("C:/Users/Elliot/AppData/Local/Microsoft/Windows/Fonts/"), prompt = F)

image_panel = function(path, path_to_czi, colors_to_return, genotype_annotation,
                       red_annotation=NA, green_annotation=NA, blue_annotation=NA, label_letters,
                       scale_bar_length=10, additional_annotation = FALSE, ...) {
  
  greyscale_annotate = function(greyscale, color_annotation, label_letters, 
                                additional_annotation = FALSE, ...){
    gg = ggdraw() + draw_image(greyscale, scale = 1)+
      theme(plot.margin = unit(c(0, 0, 0, 0), units = "in"))+
      annotate(geom = "text",  x = 0.98, y = 0.96, hjust = 1, vjust = 1,
               label = color_annotation, color = "white", size = 12/.pt)+
      annotate(geom = "text", x = 0.02, y = 0.96, hjust = 0, vjust = 1,
               label = label_letters, color = "white", size = 12/.pt)
    
    if(additional_annotation == TRUE){
      gg = gg+
        annotate(...)
    }
    return(gg)
  }
  
  rgb_image_raw = image_read(path)
  rgb_greyscales = as.list(image_separate(rgb_image_raw))
  default_color_order = c("red", "green", "blue")
  
  greyscales_pared = rgb_greyscales[default_color_order %in% colors_to_return]
  default_color_order_pared = default_color_order[default_color_order %in% colors_to_return]
  greyscales_ordered = greyscales_pared[order(factor(default_color_order_pared, levels = colors_to_return))]
  
  all_grey_annotation = list(red_annotation, green_annotation, blue_annotation)
  all_grey_annotation_ordered = all_grey_annotation[order(factor(default_color_order, levels = colors_to_return))]
  all_grey_annotation_pared = all_grey_annotation_ordered[!is.na(all_grey_annotation_ordered)][1:length(colors_to_return)]
  
  label_letters_grey = label_letters[-1]
  all_gg = mapply(FUN = greyscale_annotate, 
                  greyscale = greyscales_ordered, 
                  color_annotation = all_grey_annotation_pared, 
                  label_letters = label_letters_grey,
                  SIMPLIFY = FALSE, USE.NAMES = T, 
                  additional_annotation = additional_annotation, ...)
  
  annotation_spacer = 0
  # image_ano = rgb_image_raw
  image_ano = ggdraw() + draw_image(rgb_image_raw, scale = 1)+
    theme(plot.margin = unit(c(0, 0, 0, 0), units = "in"))
  
  if (!is.na(red_annotation)) {
    image_ano = image_ano +
      annotate(geom = "text",  x = 0.98, y = 0.96, hjust = 1, vjust = 1,
               label = red_annotation, color = "#ED1C24", size = 12/.pt)
    # image_ano = image_annotate(image_ano, text = red_annotation, gravity = "northeast", 
    #                            color = "#ED1C24", size = 12*3.402778, font = "Helvetica",
    #                            location = paste0("+", annotation_spacer))
    annotation_spacer = annotation_spacer + strwidth(paste0(red_annotation, " ") , units = "inches")/2.0694
    
  }
  if (!is.na(green_annotation)) {
    image_ano = image_ano +
      annotate(geom = "text", x = 0.98-annotation_spacer, y = 0.96, hjust = 1, vjust = 1, 
               label = green_annotation, color = "#00A651", size = 12/.pt)
    annotation_spacer = annotation_spacer + strwidth(paste0(green_annotation, " ") , units = "inches")/2.0694
  }
  if (!is.na(blue_annotation)) {
    image_ano = image_ano +
      annotate(geom = "text", x = 0.98-annotation_spacer, y = 0.96, hjust = 1, vjust = 1, 
               label = blue_annotation, color = "#1C75BC", size = 12/.pt)
  }
  
  # image_fully_ano = image_annotate(image_ano, text = genotype_annotation, gravity = "southwest", 
  #                                  color = "white", size = 12*3.402778, font = "Helvetica") %>% 
  #   image_annotate(text = label_letters[[1]], gravity = "northwest", color = "white", 
  #                  size = 12*3.402778, font = "Helvetica", location = "0")
  base_path = file_path_sans_ext(path)
  pixel_size = image_pixelsize(path_to_czi)*10^6 #in micron/px
  scale_bar_length_pixels = scale_bar_length/pixel_size
  pixel_width = read_tags(path)$frame1$width
  scale_bar_prop = scale_bar_length_pixels/pixel_width
  
  color_gg = image_ano+
    annotate(geom = "text", x = 0.02, y = 0.96, hjust = 0, vjust = 1, 
             label = label_letters[[1]], color = "white", size = 12/.pt)+
    annotate(geom = "text", x = 0.02, y = 0.04, hjust = 0, vjust = 0, 
             label = genotype_annotation, color = "white", size = 12/.pt)+
    annotate(geom = "segment", x = 0.96-scale_bar_prop, xend = 0.96, y = 0.04, yend = 0.04, color = "white")
  
  if(additional_annotation == TRUE){
    color_gg = color_gg+
      annotate(...)
  }
  
  color_gg
  all_gg$color = color_gg
  names(all_gg) = c(colors_to_return, "color")
  image_order_vector = c("color", colors_to_return)
  all_gg_ordered = all_gg[order(factor(names(all_gg), levels = image_order_vector))]

  options("multipanelfigure.defaultdpi"= 245) 
  
  figure = multi_panel_figure(
    width = rep(2.0694, length(all_gg_ordered)),
    height = c(1.1837), rows = 1, row_spacing = 0, column_spacing = 0.025, unit = "in", panel_label_type = "none")

  for (fig_index in 1:length(all_gg_ordered)) {
    figure = figure %>% fill_panel(all_gg_ordered[[fig_index]], label = "", scaling = "fit", panel_clip = "off")
  }
  return(figure)
  
}