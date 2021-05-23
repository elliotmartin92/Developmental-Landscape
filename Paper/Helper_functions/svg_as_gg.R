library(grImport)
library(ggplot2)
library(cowplot)
library(extrafont)
# Sys.setenv(R_GSCMD = normalizePath("C:/Program Files/gs/gs9.54.0/bin/gswin64c.exe"))

svg_as_gg = function(path) {
  PostScriptTrace("../Paper/Figures/Figure_1/Genetic_enrichment_cartoons.eps", charpath = FALSE)
  my_shape <- readPicture("Genetic_enrichment_cartoons.eps.xml")
  grImport::grid.picture(my_shape, gp=gpar(fontfamily="Helvetica"))
  image_gg = ggdraw() + draw_image(rgb_image_raw, scale = 1)+
    theme(plot.margin = unit(c(0, 0, 0, 0), units = "in"))
  return(image_gg)
}