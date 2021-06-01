library(magick)
library(ggplot2)
library(cowplot)

png_as_gg = function(path, plot_title=NA) {
  rgb_image_raw = image_read(path)
  image_gg = ggdraw() + draw_image(rgb_image_raw, scale = 1)+
    theme(plot.margin = unit(c(0, 0, 0, 0), units = "in"))
  if (!is.na(plot_title)) {
    title_gg = ggdraw() + 
      draw_label("test", x = 0.5)
    image_gg = plot_grid(title_gg, image_gg, ncol = 1, rel_heights = c(0.1, 1))
  }
  return(image_gg)
}
