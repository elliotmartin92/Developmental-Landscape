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

FigureS3A = gene_violin(data_set_to_plot="Input_seq", 
                         genes_by_GO="GO_term_selection", 
                        GO_term = "meiotic cell cycle",
                         normalization="each_gene",
                         text_scale = 12)+ 
  expand_limits(y = c(-4, 5))+
  ggtitle("Bulk RNA-seq: Meiotic cell cycle")+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)),
        plot.margin = margin(0,30,0,0))

# FigureS3A_input_seq_vals = read_csv("../Paper/Figures/Figure_4/Selected_gene_expression_from_Input_seq_of_GO_term_meiotic cell cycle.csv")
# 
# all_genes = read_rds("Preprocessed_data/preprocessed_RNA_seq_data.RDS")
# all_genes %>% select(1:5) %>%
#   pivot_longer(-FBGN, names_to = "Genotype", values_to = "Expression") %>% 
#   filter(Genotype == "MeanTPM_TKV_input") %>% 
#   summarise(deciles = quantile(Expression, probs = seq(.1, .9, by = 0.1)))
# 
# FigureS3A_input_seq_vals %>% 
#   group_by(Genotype) %>% 
#   dplyr::summarise(Median = median(Mean_expression))

FigureS3B = gene_violin(data_set_to_plot="Polysome_seq", 
                        genes_by_GO="GO_term_selection", 
                        GO_term = "meiotic cell cycle",
                        normalization="each_gene",
                        text_scale = 12)+ 
  expand_limits(y = c(-2, 10))+
  ggtitle("Polysome-seq: Meiotic cell cycle")+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)),
        plot.margin = margin(0,30,0,0))

FigureS3C = gene_violin(data_set_to_plot="Single_cell_seq_germline", 
                        genes_by_GO="GO_term_selection", 
                        GO_term = "meiotic cell cycle",
                        normalization="each_gene",
                        text_scale = 12)+ 
  expand_limits(y = c(-2, 2))+
  ggtitle("Meiotic cell cycle - scRNA-seq")+
  ylab("log normalized expression\nto GSC/CB/2CC")+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)),
        plot.margin = margin(0,0,0,0))

FigureS3C_SC_seq_vals = read_csv("../Paper/Figures/Figure_4/Selected_gene_expression_from_Single_cell_seq_germline_of_GO_term_meiotic cell cycle.csv")

FigureS3C_SC_seq_vals %>% 
  group_by(FBGN) %>% 
  mutate(Norm_to_1 = Mean_expression/Mean_expression[Genotype=="GSC/CB/2CC"]) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.))) %>% 
  group_by(Genotype) %>% 
  dplyr::summarise(Median = median(Norm_to_1))

FigureS3C_SC_seq_vals %>% 
  group_by(Genotype) %>% 
  dplyr::summarise(Median = median(Mean_expression))


FigureS3D = ovary_map(data_set_to_plot = "Single_cell_seq_germline",
                      gene_name_format = "Symbol",
                      displayTPM = TRUE, 
                      display_stage_labels = TRUE, 
                      display_title = TRUE,
                      gene_of_interest = "ord", 
                      text_scale = 10/ggplot2::.pt,
                      map_line_width = 0.5, 
                      graphic_to_generate = "map")+
  theme(plot.margin = margin(0,0,0,0))

FigureS3 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.025, rep(2.0694+0.025, 2), 2.025, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c(0.5, 1.1837, 1.1837, 0.2, 1.1837, 1.1837, 0.2, 1.1837, 1.1837, 1.1837, 1.1837, 0.5, (11-8*(1.1837+0.025))-0.2-0.2-0.5-0.3), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "Figure4")
FigureS3

FigureS3 = FigureS3 %>% 
  fill_panel(FigureS3A, label = "A", scaling = "fit", panel_clip = "on", row = 2:3, column = 3:6) %>% 
  fill_panel(FigureS3B, label = "B", scaling = "fit", panel_clip = "on", row = 5:6, column = 3:6) %>% 
  fill_panel(FigureS3C, label = "C", scaling = "fit", panel_clip = "on", row = 8:9, column = 3:6) %>% 
  fill_panel(FigureS3D, label = "D", scaling = "fit", panel_clip = "on", row = 10:12, column = 3:6)
FigureS3

ggsave(filename = "Supplemental_Figure3.pdf", plot = FigureS3, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
