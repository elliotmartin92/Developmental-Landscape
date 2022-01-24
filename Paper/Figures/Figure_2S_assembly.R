
if (is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
  setwd("ShinyExpresionMap/")
}else if (!is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
}else{
  errorCondition("WD is invalid")
}

library(openxlsx)
source("server_modules/violin_genes.R")
source("server_modules/ovary_map.R")

down_bam = read.xlsx(xlsxFile = "../Paper/Figures/Figure_2/bamRNAi_vs_TKV_Wilcockson_2019.xlsx", sheet = 1)
up_bam = read.xlsx(xlsxFile = "../Paper/Figures/Figure_2/bamRNAi_vs_TKV_Wilcockson_2019.xlsx", sheet = 2)

down_bam_stringent = 
  down_bam %>% 
  filter(PValue < 0.01) %>% 
  filter(logFC.bamshRNA.vs.tkvQD < -1) %>% 
  pull(ID)

up_bam_stringent = 
  up_bam %>% 
  filter(PValue < 0.01) %>% 
  filter(logFC.bamshRNA.vs.tkvQD > 1) %>% 
  pull(ID)

Figure2SA1 = gene_violin(data_set_to_plot="Input_seq", 
                        genes_by_GO="Custom_selection", 
                        gene_of_interest=down_bam_stringent, 
                        normalization="each_gene",
                        text_scale = 12)+ 
  expand_limits(y = c(-4, 3))+
  ggtitle(TeX(r'($Bulk mRNAseq: > \textit{bam}\, RNAi\, downregulated\, genes\, (Wilcockson\, 2019)$)'))+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)),
        plot.margin = margin(0,30,0,0))

Figure2SA2 = gene_violin(data_set_to_plot="Input_seq", 
                         genes_by_GO="Custom_selection", 
                         gene_of_interest=up_bam_stringent, 
                         normalization="each_gene",
                         text_scale = 12)+ 
  expand_limits(y = c(-4, 10))+
  ggtitle(TeX(r'($Bulk mRNAseq: > \textit{bam}\, RNAi\, upregulated\, genes\, (Wilcockson\, 2019)$)'))+
  theme(aspect.ratio = 0.2, 
        plot.title = element_text(size = 12, margin = margin(0,0,4,0)),
        plot.margin = margin(0,30,0,0))

Figure2SB = ovary_map(data_set_to_plot = "Polysome_seq",
                       gene_name_format = "Symbol",
                       displayTPM = TRUE, 
                       display_stage_labels = TRUE,
                       display_title = TRUE,
                       gene_of_interest = "RpS19b", 
                       text_scale = 10/ggplot2::.pt,
                       map_line_width = 0.5,
                       graphic_to_generate = "map")+
  theme(plot.margin = margin(0,60,0,0))


Figure2S = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.1388, 2.025, rep(2.0694+0.025, 2), 2.025, (8.5-4*(2.0694+0.025))/2),
  height = c(0.5, 1.1837, 1.1837, 0.25, 1.1837, 1.1837, 0.25, .25, 1.1837, 1.1837, 1.1837, 1.1837, (11-8*(1.1837+0.025))-4*(0.25)-.05), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "Figure2S")
Figure2S

Figure2S = Figure2S %>% 
  fill_panel(Figure2SA1, label = "A", scaling = "fit", panel_clip = "on", row = 2:3, column = 3:6) %>% 
  fill_panel(Figure2SA2, label = "A'", scaling = "fit", panel_clip = "on", row = 5:6, column = 3:6) %>% 
  fill_panel(Figure2SB, label = "B", scaling = "fit", panel_clip = "on", row = 8:10, column = 3:6)
Figure2S

ggsave(filename = "Supplemental_Figure2.pdf", plot = Figure2S, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
