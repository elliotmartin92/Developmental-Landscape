setwd("ShinyExpresionMap")
library(ggplot2)
library(multipanelfigure)
library(pdftools)
library(extrafont)
source("../Paper/Helper_functions/image_panel.R")
source("../Paper/Helper_functions/eps_as_gg.R")
source("../Paper/Helper_functions/png_as_gg.R")
# font_import(paths = c("C:/Users/Elliot/AppData/Local/Microsoft/Windows/Fonts/"), prompt = F)

source("server_modules/ovary_map_cartoon.R")
Figure1_A1 =  ovary_map_cartoon(text_scale = 12/ggplot2::.pt)+
  theme(aspect.ratio = .28,
        plot.margin = margin(0.0, 0.0, 0.1, 0.0, unit = "in"))

Figure1_B1 = eps_as_gg("../Paper/Figures/Figure_1/Genetic_enrichment_cartoons.eps")
Figure1_C = eps_as_gg("../Paper/Figures/Figure_1/polysome_seq_diagram.eps")

# Figure1_D_paths = list.files(path = "../Paper/Figures/Figure_1/", pattern = "*heatmap.png", full.names = TRUE)
# Figure1_D = lapply(Figure1_D_paths, png_as_gg)

Figure1 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.0, rep(2.0694, 2), 2.0, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c((11-8*(1.1837+0.025))/2-.3, 1.4837, rep(1.1837, 6), 1.4837, (11-8*(1.1837+0.025))/2-.3), row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "RpS19b_control_grouped")
Figure1

# plotly_IMAGE(
#   input_plotly,
#   width = 500,
#   height = 500,
#   format = "png",
#   scale = 1,
#   "plotly_input.png"
# )

Figure1 = Figure1 %>% 
  fill_panel(Figure1_A1, label = "A", label_just = "bottom", scaling = "none", panel_clip = "off", row = 2:3, column = 3:6, family = "Helvetica") %>% 
  fill_panel(Figure1_B1, label = "B", label_just = "bottom", scaling = "none", panel_clip = "off", row = 4:6, column = 3:6, family = "Helvetica") %>%
  fill_panel(Figure1_C, label = "C", label_just = "bottom", scaling = "none", panel_clip = "off", row = 7:8, column = 3:5, family = "Helvetica")
  # fill_panel(Figure1_D[[1]], label = "D", label_just = "bottom", scaling = "none", panel_clip = "off", row = 5:10, column = 3:6, family = "Helvetica")
  # fill_panel(Figure1_D[[2]], label = "D'", label_just = "bottom", scaling = "none", panel_clip = "off", row = 9:10, column = 5:6, family = "Helvetica") %>% 
  # fill_panel(Figure1_D[[3]], label = "E", label_just = "bottom", scaling = "none", panel_clip = "off", row = 11:12, column = 3:4, family = "Helvetica") %>% 
  # fill_panel(Figure1_D[[4]], label = "E'", label_just = "bottom", scaling = "none", panel_clip = "off", row = 11:12, column = 5:6, family = "Helvetica")

Figure1

ggsave(filename = "Figure1.pdf", plot = Figure1, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
