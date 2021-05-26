library(ggplot2)
library(cowplot)
library(magick)
# Sys.setenv(R_GSCMD = normalizePath("C:/Program Files/gs/gs9.54.0/bin/gswin64c.exe"))

svg_as_gg = function(path, width, height) {
  raw_svg = magick::image_read_svg(path, width = width, height = height)
  image_gg = ggdraw() + draw_image(raw_svg, scale = 1)+
    theme(plot.margin = unit(c(0, 0, 0, 0), units = "in"))
  return(image_gg)
}


