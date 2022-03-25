
if (is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
  setwd("ShinyExpresionMap/")
}else if (!is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
}else{
  errorCondition("WD is invalid")
}

library(ggplot2)
library(multipanelfigure)
library(pdftools)
library(extrafont)
source("../Paper/Helper_functions/image_panel.R")
source("../Paper/Helper_functions/eps_as_gg.R")
source("../Paper/Helper_functions/png_as_gg.R")
source("../Paper/Helper_functions/export_plotly2SVG.R")

Figure1S_A = eps_as_gg("../Paper/Figures/Figure_1/polysome_seq_diagram.eps")


Figure1S_C_paths = list.files(path = "../Paper/Figures/Figure_1/", pattern = "*heatmap.RDS.png", full.names = TRUE)
Figure1S_C = lapply(Figure1S_C_paths, png_as_gg)

Figure1S = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, rep(2.0694, 2), 0.6, 1.4694, 2.0694, (8.5-4*(2.0694+0.025))/2),
  height = c((11-0.5-8*(1.1837+0.05))/2, rep(1.1837, 2), 0.5, rep(1.1837, 6), (11-0.5-8*(1.1837+0.05))/2), 
  row_spacing = 0.05, column_spacing = 0.025, unit = "in", 
  panel_label_type = "none", figure_name = "Supplemental_Figure1")
# Figure1S

Figure1S = Figure1S %>% 
  fill_panel(Figure1S_A, label = "A", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 2:3, column = 2:4) %>% 
  fill_panel(Figure1S_C[[1]], label = "B",
             label_just = "bottom", scaling = "fit", panel_clip = "off", row = 5:7, column = 2:3) %>% 
  fill_panel(Figure1S_C[[2]], label = "B'", 
             label_just = "bottom", scaling = "fit", panel_clip = "off", row = 5:7, column = 4:6) %>% 
  fill_panel(Figure1S_C[[3]], label = "C", 
             label_just = "bottom", scaling = "fit", panel_clip = "off", row = 8:10, column = 2:3) %>%  
  fill_panel(Figure1S_C[[4]], label = "C'", 
             label_just = "bottom", scaling = "fit", panel_clip = "off", row = 8:10, column = 4:6)
Figure1S

ggsave(filename = "Supplemental_Figure1.pdf", plot = Figure1S, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
