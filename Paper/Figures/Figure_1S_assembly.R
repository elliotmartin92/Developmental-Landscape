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


plotly_files = c("Preprocessed_data/Input_seq_plotly_heatmap.RDS",
                 "Preprocessed_data/Polysome_seq_plotly_heatmap.RDS",
                 "Preprocessed_data/Single_cell_seq_germline_plotly_heatmap.RDS",
                 "Preprocessed_data/Single_cell_seq_soma_plotly_heatmap.RDS")

plotly_file_names = lapply(plotly_files, basename)
input_plotlys = lapply(plotly_files, readRDS)
mapply(export_plotly2SVG, input_plotlys, filename = plotly_file_names,
       parent_path = "../Paper/Figures/Figure_1/", width = 300, height = 250)
# had to manually adust svgs and export as png

Figure1S_C_paths = list.files(path = "../Paper/Figures/Figure_1/", pattern = "*heatmap.RDS.png", full.names = TRUE)
# Figure1_C = lapply(Figure1_C_paths, eps_as_gg)
Figure1S_C = lapply(Figure1S_C_paths, png_as_gg)

Figure1S = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, rep(2.0694, 4), (8.5-4*(2.0694+0.025))/2),
  height = c((11-0.5-8*(1.1837+0.05))/2, rep(1.1837, 2), 0.5, rep(1.1837, 6), (11-0.5-8*(1.1837+0.05))/2), 
  row_spacing = 0.05, column_spacing = 0.025, unit = "in", 
  panel_label_type = "none", figure_name = "Supplemental_Figure1")
Figure1S

Figure1S = Figure1S %>% 
  fill_panel(Figure1S_A, label = "A", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 2:3, column = 2:6) %>% 
  fill_panel(Figure1S_C[[1]], 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 5:7, column = 2:3) %>% 
  fill_panel(Figure1S_C[[2]], label = "B'", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 5:7, column = 4:5) %>% 
  fill_panel(Figure1S_C[[3]], label = "C", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 8:10, column = 2:3) %>%  
  fill_panel(Figure1S_C[[4]], label = "C'", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 8:10, column = 4:5)
Figure1S

ggsave(filename = "Supplemental_Figure1.pdf", plot = Figure1S, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
