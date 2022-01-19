
if (is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
  setwd("ShinyExpresionMap/")
}else if (!is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
}else{
  errorCondition("WD is invalid")
}

source("../Paper/Helper_functions/image_panel.R")
source("../Paper/Helper_functions/png_as_gg.R")
source("server_modules/ovary_map.R")

Figure2A1 = ovary_map(data_set_to_plot = "Input_seq",
                      gene_name_format = "Symbol",
                      displayTPM = TRUE, 
                      display_stage_labels = TRUE, 
                      display_title = TRUE,
                      gene_of_interest = "RpS19b", 
                      text_scale = 10/ggplot2::.pt,
                      map_line_width = 0.5, 
                      graphic_to_generate = "map")

Figure2A2 = ovary_map(data_set_to_plot = "Single_cell_seq_germline",
                      gene_name_format = "Symbol",
                      displayTPM = TRUE, 
                      display_stage_labels = TRUE,
                      display_title = TRUE,
                      gene_of_interest = "RpS19b", 
                      text_scale = 10/ggplot2::.pt,
                      map_line_width = 0.5,
                      graphic_to_generate = "map")

Figure2B = image_panel(path = "../Paper/Figures/Figure_2/RpS19b_in_situ_DAPI_Image7_s10.tif",
                       path_to_czi = "../Paper/Figures/Figure_2/RpS19b_in_situ_DAPI_Image7.czi",
                    colors_to_return = c("green", "blue"), 
                    genotype_annotation = "Control",
                    green_annotation = "RpS19b mRNA", 
                    blue_annotation = "DAPI",
                    label_letters = c("C", "C'", "C''"),
                    scale_bar_length = 20, 
                    additional_annotation = TRUE, 
                    geom = "segment", x = .1, xend=.22, y=.35, yend=.42, color="gold")
Figure2B

Figure2C = image_panel(path = "../Paper/Figures/Figure_2/RpS19b_GFP_1B1_GFP_Vas_Image 11_s4_6.tif", 
                       path_to_czi = "../Paper/Figures/Figure_2/RpS19b_GFP_1B1_GFP_Vas_Image 11_s4_6 Image 11.czi",
                    colors_to_return = c("green", "blue"), 
                    genotype_annotation = "RpS19b::GFP",
                    red_annotation = "1B1", 
                    green_annotation = "GFP", 
                    blue_annotation = "Vasa",
                    label_letters = c("D", "D'", "D''"),
                    scale_bar_length = 20, 
                    additional_annotation = TRUE, 
                    geom = "segment", x = .08, xend=.3, y=.32, yend=.28, color="gold")
Figure2C

Figure2 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.025, rep(2.0694+0.025, 2), 2.025, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c(0.25, 0.25, 1.1837, 1.1837, 0.25, 1.1837, 1.1837, 0.25, 1.1837, 1.1837, 1.1837, 1.1837, (11-8*(1.1837+0.025))-0.25-0.25-0.25-0.25), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "Figure2")
Figure2

Figure2 = Figure2 %>% 
  fill_panel(Figure2A1, label = "A", scaling = "fit", panel_clip = "on", row = 2:4, column = 3:6) %>% 
  fill_panel(Figure2A2, label = "B", scaling = "fit", panel_clip = "on", row = 6:7, column = 3:6) %>% 
  fill_panel(Figure2B, label = "", scaling = "none", panel_clip = "off", row = 9, column = 2:5) %>% 
  fill_panel(Figure2C, label = "", scaling = "none", panel_clip = "off", row = 10, column = 2:5)

Figure2

ggsave(filename = "Figure2.pdf", plot = Figure2, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
