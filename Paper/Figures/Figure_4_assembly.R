
if (is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
  setwd("ShinyExpresionMap/")
}else if (!is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
}else{
  errorCondition("WD is invalid")
}

source("../Paper/Helper_functions/image_panel.R")
source("../Paper/Helper_functions/png_as_gg.R")
source("server_modules/ggplotWhiteTheme.R")
source("server_modules/ovary_map.R")
source("../Paper/Helper_functions/in_situ_quant.r")

Figure4A = ovary_map(data_set_to_plot = "Input_seq",
                      gene_name_format = "Symbol",
                      displayTPM = TRUE, 
                      display_stage_labels = TRUE, 
                      display_title = TRUE,
                      gene_of_interest = "ord",
                      text_scale = 10/ggplot2::.pt,
                      map_line_width = 0.5, 
                      graphic_to_generate = "map")+
  theme(plot.margin = margin(c(0,60,5,0)))

Figure4B = ovary_map(data_set_to_plot = "Polysome_seq",
                      gene_name_format = "Symbol",
                      displayTPM = TRUE, 
                      display_stage_labels = TRUE, 
                      display_title = TRUE,
                      gene_of_interest = "ord", 
                      text_scale = 10/ggplot2::.pt,
                      map_line_width = 0.5, 
                      graphic_to_generate = "map")+
  theme(plot.margin = margin(c(5,60,0,0)))

Figure4C = image_panel(path = "../Paper/Figures/Figure_4/img2_24_RGB ps.tif", 
                    path_to_czi = "../Paper/Figures/Figure_4/Image 2.czi",
                    colors_to_return = c("red", "green"), 
                    genotype_annotation = "ord::GFP",
                    green_annotation = "GFP", 
                    red_annotation = "gfp mRNA",
                    blue_annotation = "DAPI",
                    label_letters = c("C", "C'", "C''"),
                    scale_bar_length = 20)

Figure4D1 = plot_insitu_quant(staining_to_plot = "mRNA", 
                              xlsx_file = "../Paper/Figures/Figure_4/Ord_GFP_in_situ_quant_Kahini.xlsx",
                              gene_name = "Ord")

Figure4D2 = plot_insitu_quant(staining_to_plot = "protein", 
                              xlsx_file = "../Paper/Figures/Figure_4/Ord_GFP_in_situ_quant_Kahini.xlsx",
                              gene_name = "Ord")
# plot_ord_quant("TE")

# Figure4D = image_panel(path = "../Paper/Figures/Figure_4/control_C3Gprot_C3Grna_Vasa_11_s6_8.tif", 
#                        path_to_czi = "../Paper/Figures/Figure_4/Image 11.czi",
#                        colors_to_return = c("red", "green"), 
#                        genotype_annotation = "Control",
#                        green_annotation = "C(3)G", 
#                        red_annotation = "c(3)G mRNA",
#                        blue_annotation = "Vasa",
#                        label_letters = c("D", "D'", "D''"),
#                        scale_bar_length = 20)


Figure4 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.025, rep(2.0694+0.025, 2), 2.025, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c(0.5, 0.25, 1.1837, 1.1837, 0.25, 1.1837, 1.1837, 0.25, 1.1837, 0.25, 1.1837, 1.1837, 1.1837, (11-8*(1.1837+0.025))-(5*0.25)), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "Figure4")
Figure4

Figure4 = Figure4 %>% 
  fill_panel(Figure4A, label = "A", scaling = "fit", panel_clip = "on", row = 2:4, column = 3:6) %>% 
  fill_panel(Figure4B, label = "B", scaling = "fit", panel_clip = "on", row = 5:7, column = 3:6) %>% 
  fill_panel(Figure4C, label = "", scaling = "none", panel_clip = "off", row = 9, column = 2:5) %>% 
  fill_panel(Figure4D1, label = "D", scaling = "fit", panel_clip = "off", row = 11:12, column = 3:4) %>% 
  fill_panel(Figure4D2, label = "D'", scaling = "fit", panel_clip = "off", row = 11:12, column = 5:6)
Figure4

ggsave(filename = "Figure4.pdf", plot = Figure4, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
