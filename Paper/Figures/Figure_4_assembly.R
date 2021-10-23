
if (is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
  setwd("ShinyExpresionMap/")
}else if (!is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
}else{
  errorCondition("WD is invalid")
}

source("../Paper/Helper_functions/image_panel.R")
source("../Paper/Helper_functions/png_as_gg.R")
source("server_modules/ggplotWhiteTheme.R")
source("server_modules/violin_genes.R")

Figure4A1 = gene_violin(data_set_to_plot="Input_seq", 
                       genes_by_GO="GO_term_selection", 
                       GO_term="double-strand break repair", 
                       gene_of_interest=NA, 
                       normalization="each_gene",
                       12)+ 
  expand_limits(y = c(-4, 2.5))+
  ggtitle("GO term: double-strand break repair")+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)))

Figure4A2 = gene_violin(data_set_to_plot="Polysome_seq", 
                        genes_by_GO="GO_term_selection", 
                        GO_term="double-strand break repair", 
                        gene_of_interest=NA, 
                        normalization="each_gene",
                        12)+ 
  expand_limits(y = c(-2, 3.5))+
  ggtitle("GO term: double-strand break repair")+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)))


Fig4B = image_panel(path = "../Paper/Figures/placeholder.tif", 
                    path_to_czi = "../Paper/Figures/Figure_3/Control.Rps19b-GFP.40x.4.czi",
                    colors_to_return = c("green", "blue"), 
                    genotype_annotation = "Control",
                    green_annotation = "Brca2 mRNA", 
                    blue_annotation = "Vasa",
                    label_letters = c("C", "C'", "C''"),
                    scale_bar_length = 20)

Fig4C = image_panel(path = "../Paper/Figures/placeholder.tif", 
                    path_to_czi = "../Paper/Figures/Figure_3/Control.Rps19b-GFP.40x.4.czi",
                    colors_to_return = c("green", "blue"), 
                    genotype_annotation = "Control",
                    green_annotation = "Brca2 translation", 
                    blue_annotation = "Vasa",
                    label_letters = c("D", "D'", "D''"),
                    scale_bar_length = 20)

Figure4 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.2, 1.7944, 2.0944, 2.0944, 2.0944, (8.5-4*(2.0694+0.025))/2),
  height = c((11-8*(1.1837+0.025))/2-.5, rep(1.1837, 2), 0.2, rep(1.1837, 2), 0.2, rep(1.1837, 4), (11-8*(1.1837+0.025))/2-.5), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "Figure4")
Figure4

Figure4 = Figure4 %>% 
  fill_panel(Figure4A1, label = "A", scaling = "fit", panel_clip = "on", row = 2:3, column = 3:6) %>% 
  fill_panel(Figure4A2, label = "B", scaling = "fit", panel_clip = "on", row = 5:6, column = 3:6) %>% 
  fill_panel(Fig4B, label = "", scaling = "none", panel_clip = "off", row = 8, column = 2:5) %>% 
  fill_panel(Fig4C, label = "", scaling = "none", panel_clip = "off", row = 9, column = 2:5)

Figure4

ggsave(filename = "Figure4.pdf", plot = Figure4, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
