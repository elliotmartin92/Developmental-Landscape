setwd("ShinyExpresionMap")
library(ggplot2)
library(multipanelfigure)
library(pdftools)
source("../Paper/Helper_functions/image_panel.R")
source("../Paper/Helper_functions/png_as_gg.R")
font_import(paths = c("C:/Users/Elliot/AppData/Local/Microsoft/Windows/Fonts/"), prompt = F)

# Figure1_A1 =  png_as_gg("../Paper/Figures/Figure_1/Germarium_cartoon.png")
source("server_modules/ovary_map_cartoon.R")
Figure1_A1 =  ovary_map_cartoon(text_scale = 12/ggplot2::.pt) +
  theme(aspect.ratio = .3,
        plot.margin = margin(0.0, 0.05, 0.2, 0.05, unit = "in"))
Figure1_B1 =  png_as_gg("../Paper/Figures/Figure_1/Genetic_enrichment_cartoons.png")
Figure1_C = png_as_gg("../Paper/Figures/Figure_1/polysome_seq_diagram.png")

Figure1_D_paths = list.files(path = "../Paper/Figures/Figure_1/", pattern = "*heatmap.png", full.names = TRUE)
Figure1_D = lapply(Figure1_D_paths, png_as_gg)


Figure1 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.0, rep(2.0694, 2), 2.0, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c((11-9*(1.1837+0.025))/2, 0.1837, 1, rep(1.1837, 7), 1, 0.1837, (11-9*(1.1837+0.025))/2), row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "RpS19b_control_grouped")
Figure1

Figure1 = Figure1 %>% 
  fill_panel(Figure1_A1, label = "A", label_just = "bottom", scaling = "none", panel_clip = "off", row = 2:4, column = 3:6, family = "Helvetica") %>% 
  fill_panel(Figure1_B1, label = "B", label_just = "bottom", scaling = "none", panel_clip = "off", row = 5:7, column = 3:6, family = "Helvetica") %>% 
  fill_panel(Figure1_C, label = "C", label_just = "bottom", scaling = "none", panel_clip = "off", row = 8:9, column = 3:5, family = "Helvetica") 
  # fill_panel(Figure1_D[[1]], label = "D", label_just = "bottom", scaling = "none", panel_clip = "off", row = 9:10, column = 3:4, family = "Helvetica") %>% 
  # fill_panel(Figure1_D[[2]], label = "D'", label_just = "bottom", scaling = "none", panel_clip = "off", row = 9:10, column = 5:6, family = "Helvetica") %>% 
  # fill_panel(Figure1_D[[3]], label = "E", label_just = "bottom", scaling = "none", panel_clip = "off", row = 11:12, column = 3:4, family = "Helvetica") %>% 
  # fill_panel(Figure1_D[[4]], label = "E'", label_just = "bottom", scaling = "none", panel_clip = "off", row = 11:12, column = 5:6, family = "Helvetica")

Figure1

ggsave(filename = "Figure1.pdf", plot = Figure1, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
