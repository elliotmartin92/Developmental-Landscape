source("Paper/Helper_functions/image_panel.R")
source("Paper/Helper_functions/png_as_gg.R")

Figure2A1 = png_as_gg("Paper/Figures/Figure_2/RpS19b_polysome.png")
Figure2A2 = png_as_gg("Paper/Figures/Figure_2/RpS19b_SC_soma.png")


Figure2 = multi_panel_figure(
  width = c((8.3-4*2.0694)/2, rep(2.0694, 4), (8.3-4*2.0694)/2),
  height = c((10.8-9*1.1837)/3, rep(1.1837, 9), (10.8-9*1.1837)/2), row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "RpS19b_control_grouped")
Figure2

Figure2 = Figure2 %>% 
  fill_panel(Figure2A1, label = "A", scaling = "fit", panel_clip = "on", row = 2:3, column = 2:5) %>% 
  fill_panel(Figure2A2, label = "B", scaling = "fit", panel_clip = "on", row = 4:5, column = 2:5)

Figure2

ggsave(filename = "Supplemental_Figure2.pdf", plot = Figure2, path = "Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
