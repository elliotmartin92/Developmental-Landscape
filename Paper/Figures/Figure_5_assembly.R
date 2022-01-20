
if (is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
  setwd("ShinyExpresionMap/")
}else if (!is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
}else{
  errorCondition("WD is invalid")
}

# source("../Paper/Helper_functions/image_panel.R")
# source("../Paper/Helper_functions/png_as_gg.R")
source("server_modules/ggplotWhiteTheme.R")
source("server_modules/violin_genes.R")
# source("server_modules/ovary_map.R")

Figure5A = gene_violin(data_set_to_plot="Input_seq", 
                        genes_by_GO="GO_term_selection", 
                        GO_term = "double-strand break repair",
                        normalization="each_gene",
                        text_scale = 12)+ 
  expand_limits(y = c(-4, 2.5))+
  ggtitle("Double-strand break repair - Input")+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)),
        plot.margin = margin(0,0,0,0))

Figure5B = gene_violin(data_set_to_plot="Polysome_seq", 
                       genes_by_GO="GO_term_selection", 
                       GO_term = "double-strand break repair",
                       normalization="each_gene",
                       text_scale = 12)+ 
  expand_limits(y = c(-3, 4))+
  ggtitle("Double-strand break repair - Polysome")+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)),
        plot.margin = margin(0,0,0,0))

Figure5C = gene_violin(data_set_to_plot="Single_cell_seq_germline", 
                       genes_by_GO="GO_term_selection", 
                       GO_term = "double-strand break repair",
                       normalization="each_gene",
                       text_scale = 12)+ 
  expand_limits(y = c(-1, 1.2))+
  ggtitle("Double-strand break repair - sc-RNAseq")+
  ylab("log normalized expression\ntoGSC/CB/2CC")+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)),
  plot.margin = margin(0,0,0,0))

Figure5 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.025, rep(2.0694+0.025, 2), 2.025, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c(0.5, 1.1837, 1.1837, 0.25, 1.1837, 1.1837, 0.25, 1.1837, 1.1837, 1.1837, 1.1837, (11-8*(1.1837+0.025))-0.25-0.25), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "Figure5")
Figure5

Figure5 = Figure5 %>% 
  fill_panel(Figure5A, label = "A", scaling = "fit", panel_clip = "on", row = 2:3, column = 3:6) %>% 
  fill_panel(Figure5B, label = "B", scaling = "fit", panel_clip = "on", row = 5:6, column = 3:6) %>%
  fill_panel(Figure5C, label = "C", scaling = "fit", panel_clip = "on", row = 8:10, column = 3:6)
Figure5

ggsave(filename = "Figure5.pdf", plot = Figure5, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
