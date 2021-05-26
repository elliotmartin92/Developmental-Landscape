setwd("ShinyExpresionMap")
library(ggplot2)
library(multipanelfigure)
library(pdftools)
library(extrafont)
source("../Paper/Helper_functions/image_panel.R")
source("../Paper/Helper_functions/eps_as_gg.R")
source("../Paper/Helper_functions/png_as_gg.R")
# font_import(paths = c("C:/Users/Elliot/AppData/Local/Microsoft/Windows/Fonts/"), prompt = F)

Figure1S_A = eps_as_gg("../Paper/Figures/Figure_1/polysome_seq_diagram.eps")

Figure1S_C_paths = list.files(path = "../Paper/Figures/Figure_1/", pattern = "*heatmap.RDS.png", full.names = TRUE)
# Figure1_C = lapply(Figure1_C_paths, eps_as_gg)
Figure1S_C = lapply(Figure1_C_paths, png_as_gg)

Figure1S = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.0, rep(2.0694, 2), 2.0, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c((11-8*(1.1837+0.025))/2-.2, 1.4837, 1.0, 0.1837, rep(1.1837, 5), 1.0, 0.4837, (11-8*(1.1837+0.025))/2-.2), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "Supplemental_Figure1")
Figure1S

Figure1S = Figure1S %>% 
  fill_panel(Figure1S_A, label = "A", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 2:3, column = 3:6, family = "Helvetica") %>% 
  fill_panel(Figure1S_C[[3]], label = "B", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 5:8, column = 3:4, family = "Helvetica") %>%  
  fill_panel(Figure1S_C[[4]], label = "B'", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 5:8, column = 5:6, family = "Helvetica")
Figure1S

ggsave(filename = "Supplemental_Figure1.pdf", plot = Figure1S, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
