if (is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
  setwd("ShinyExpresionMap")
}else if (strsplit(getwd(), "Developmental-Landscape")[[1]][2] == "/ShinyExpresionMap") {
  warning("WD already set to /ShinyExpresionMap")
}else{
  errorCondition("WD is invalid")
}

source("server_modules/ggplotWhiteTheme.R")
source("server_modules/violin_genes.R")
source("server_modules/ovary_map.R")

FigureS4A = gene_violin(data_set_to_plot="Input_seq", 
                         genes_by_GO="GO_term_selection", 
                        GO_term = "meiosis I",
                         normalization="each_gene",
                         text_scale = 12)+ 
  expand_limits(y = c(-4, 3))+
  ggtitle("Meiosis I - Input")+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)))

FigureS4B = gene_violin(data_set_to_plot="Polysome_seq", 
                        genes_by_GO="GO_term_selection", 
                        GO_term = "meiosis I",
                        normalization="each_gene",
                        text_scale = 12)+ 
  expand_limits(y = c(-2, 10))+
  ggtitle("Meiosis I - Polysome")+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)))

FigureS4C = gene_violin(data_set_to_plot="Single_cell_seq_germline", 
                        genes_by_GO="GO_term_selection", 
                        GO_term = "meiosis I",
                        normalization="each_gene",
                        text_scale = 12)+ 
  expand_limits(y = c(-2, 2))+
  ggtitle("Meiosis I - sc-RNAseq")+
  ylab("log normalized expression\ntoGSC/CB/2CC")+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)))

FigureS4C_SC_seq_vals = read_csv("../Paper/Figures/Figure_4/Selected_gene_expression_from_Single_cell_seq_germline_of_GO_term_meiosis I.csv")
FigureS4C_SC_seq_vals %>% group_by(Genotype) %>% summarise(Mean = mean(Norm_expression))

FigureS4D = ovary_map(data_set_to_plot = "Single_cell_seq_germline",
                      gene_name_format = "Symbol",
                      displayTPM = TRUE, 
                      display_stage_labels = TRUE, 
                      display_title = TRUE,
                      gene_of_interest = "ord", 
                      text_scale = 10/ggplot2::.pt,
                      map_line_width = 0.5, 
                      graphic_to_generate = "map")

FigureS4 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.025, rep(2.0694+0.025, 2), 2.025, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c(0.45, 1.1837, 1.1837, 0.25, 1.1837, 1.1837, 0.25, 1.1837, 1.1837, 0.25, 1.1837, 1.1837, (11-8*(1.1837+0.025))-0.25-0.25-0.25-0.25), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "Figure4")
FigureS4

FigureS4 = FigureS4 %>% 
  fill_panel(FigureS4A, label = "A", scaling = "fit", panel_clip = "on", row = 2:3, column = 3:5) %>% 
  fill_panel(FigureS4B, label = "B", scaling = "fit", panel_clip = "on", row = 5:6, column = 3:5) %>% 
  fill_panel(FigureS4C, label = "C", scaling = "fit", panel_clip = "on", row = 7:10, column = 3:7) %>% 
  fill_panel(FigureS4D, label = "D", scaling = "fit", panel_clip = "on", row = 11:12, column = 3:6)
FigureS4

ggsave(filename = "Supplemental_Figure4.pdf", plot = FigureS4, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
