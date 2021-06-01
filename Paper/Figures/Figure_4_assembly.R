setwd("ShinyExpresionMap")
library(tidyverse)
source("../Paper/Helper_functions/image_panel.R")
source("../Paper/Helper_functions/png_as_gg.R")

# Figure4A1 = png_as_gg("Paper/Figures/Figure_4/RpS19b_input.png")
# Figure4A2 = png_as_gg("../Paper/Figures/Figure_4/RpS19b_SC_GC.png")

source("server_modules/violin_genes.R")
Figure4A1 = gene_violin(data_set_to_plot="Input_seq", 
                       genes_by_GO="GO_term_selection", 
                       GO_term="double-strand break repair", 
                       gene_of_interest=NA, 
                       normalization="each_gene",
                       12)+ 
  expand_limits(y = c(-4, 2.5))

Figure4A2 = gene_violin(data_set_to_plot="Polysome_seq", 
                        genes_by_GO="GO_term_selection", 
                        GO_term="double-strand break repair", 
                        gene_of_interest=NA, 
                        normalization="each_gene",
                        12)+ 
  expand_limits(y = c(-2, 3.5))

Fig4B = image_panel(path = "../Paper/Figures/placeholder.tif", 
                    colors_to_return = c("green", "blue"), 
                    genotype_annotation = "Control",
                    green_annotation = "Brca2 mRNA", 
                    blue_annotation = "Vasa",
                    label_letters = c("C", "C'", "C''"))

Fig4C = image_panel(path = "../Paper/Figures/placeholder.tif", 
                    colors_to_return = c("green", "blue"), 
                    genotype_annotation = "Control",
                    green_annotation = "Brca2 translation", 
                    blue_annotation = "Vasa",
                    label_letters = c("D", "D'", "D''"))

Figure4 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.025, rep(2.0694+0.025, 2), 2.025, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c((11-8*(1.1837+0.025))/2-.3, 1.4837, rep(1.1837, 6), 1.4837, (11-8*(1.1837+0.025))/2-.3), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "RpS19b_control_grouped")
Figure4

Figure4 = Figure4 %>% 
  fill_panel(Figure4A1, label = "A", scaling = "fit", panel_clip = "on", row = 2:3, column = 3:6) %>% 
  fill_panel(Figure4A2, label = "B", scaling = "fit", panel_clip = "on", row = 4:5, column = 3:6) %>% 
  fill_panel(Fig4B, label = "", scaling = "none", panel_clip = "off", row = 6, column = 2:5) %>% 
  fill_panel(Fig4C, label = "", scaling = "none", panel_clip = "off", row = 7, column = 2:5)

Figure4

ggsave(filename = "Figure4.pdf", plot = Figure4, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
