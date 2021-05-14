library(magick)
library(patchwork)

image_panel = function(path, colors_to_return, genotype_annotation, main_annotation, red_annotation, green_annotation, blue_annotation) {
  rgb_image_raw = image_read(path)
  test = image_ggplot(rgb_image_raw)
  test

  rgb_image = image_scale(rgb_image_raw, "29.38778988028459%")
  
  if ("red" %in% colors_to_return) {
    image_red = image_separate(rps19b_protein_scale)[1]
    red_ano = image_annotate(image_red, text = red_annotation, gravity = "northeast", 
                               color = "white", size = 12, font = "Helvetica")
  }
  if ("green" %in% colors_to_return) {
    image_green = image_separate(rps19b_protein_scale)[2]
    green_ano = image_annotate(image_green, text = green_annotation, gravity = "northeast", 
                             color = "white", size = 12, font = "Helvetica")
  }
  if ("blue" %in% colors_to_return) {
    image_blue = image_separate(rps19b_protein_scale)[3]
    blue_ano = image_annotate(image_blue, text = blue_annotation, gravity = "northeast", 
                             color = "white", size = 12, font = "Helvetica")
  }
  
  color_annotation = paste(red_annotation, green_annotation, blue_annotation)
  
  red_annotation_width = strwidth(red_annotation, font = 12, units = "figure")*72*6
  green_annotation_width = strwidth(green_annotation, font = 12, units = "figure")*72*6
  red_green_annotation_width = red_annotation_width+green_annotation_width
  
  image_ano = image_annotate(rgb_image, text = red_annotation, gravity = "northeast", 
                                      color = "#ED1C24", size = 12, font = "Helvetica") %>% 
                  image_annotate(text = green_annotation, gravity = "northeast", location = paste0("+", red_annotation_width),
                                 color = "#00A651", size = 12, font = "Helvetica") %>% 
                  image_annotate(text = blue_annotation, gravity = "northeast", location = paste0("+", red_green_annotation_width),
                   color = "#00AEEF", size = 12, font = "Helvetica")
  
  image_fully_ano = image_annotate(image_ano, text = genotype_annotation, gravity = "southwest", 
                   color = "white", size = 12, font = "Helvetica")
  
  test = image_montage(c(image_fully_ano, red_ano, green_ano, blue_ano), geometry = "x100+3+3")
  return(test)
}

image_panel(path = "Paper/Figures/Figure 1/Control.Rps19b-GFP.40x.4_s3_5.tif", 
            colors_to_return = c("red", "green", "blue"), 
            genotype_annotation = "RpS19b::GFP",
            red_annotation = "1B1", green_annotation = "RpS19 ", blue_annotation = "Vasa")
