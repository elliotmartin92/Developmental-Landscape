setwd("ShinyExpresionMap")
source("../Paper/Helper_functions/image_panel.R")
source("../Paper/Helper_functions/png_as_gg.R")

source("server_modules/ovary_map.R")

Figure2SA1 = ovary_map(data_set_to_plot = "Polysome_seq",
                      gene_name_format = "Symbol",
                      displayTPM = TRUE, 
                      display_stage_labels = TRUE,
                      gene_of_interest = "RpS19b", 
                      text_scale = 10/ggplot2::.pt,
                      map_line_width = 0.5,
                      graphic_to_generate = "map")

Figure2SA2 = ovary_map(data_set_to_plot = "Single_cell_seq_soma",
                      gene_name_format = "Symbol",
                      displayTPM = TRUE, 
                      display_stage_labels = TRUE,
                      gene_of_interest = "RpS19b", 
                      text_scale = 10/ggplot2::.pt,
                      map_line_width = 0.5,
                      graphic_to_generate = "map")


Figure2 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.025, rep(2.0694+0.025, 2), 2.025, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c((11-8*(1.1837+0.025))/2-.3, 1.4837, rep(1.1837, 6), 1.4837, (11-8*(1.1837+0.025))/2-.3), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "RpS19b_control_grouped")
Figure2

Figure2 = Figure2 %>% 
  fill_panel(Figure2SA1, label = "A", scaling = "fit", panel_clip = "on", row = 2:3, column = 3:6) %>% 
  fill_panel(Figure2SA2, label = "B", scaling = "fit", panel_clip = "on", row = 4:5, column = 3:6)

Figure2

ggsave(filename = "Supplemental_Figure2.pdf", plot = Figure2, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
