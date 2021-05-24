library(magick)
library(ggplot2)
library(cowplot)

png_as_gg = function(path) {
  rgb_image_raw = image_read(path)
  image_gg = ggdraw() + draw_image(rgb_image_raw, scale = 1)+
    theme(plot.margin = unit(c(0, 0, 0, 0), units = "in"))
  return(image_gg)
}