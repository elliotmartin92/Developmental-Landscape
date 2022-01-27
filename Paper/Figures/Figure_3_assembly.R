
if (is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
  setwd("ShinyExpresionMap/")
}else if (!is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
}else{
  errorCondition("WD is invalid")
}

library(tidyverse)
library(ggplot2)
library(multipanelfigure)
library(pdftools)
library(extrafont)
source("../Paper/Helper_functions/image_panel.R")
source("../Paper/Helper_functions/eps_as_gg.R")
source("../Paper/Helper_functions/png_as_gg.R")
source("../Paper/Helper_functions/export_plotly2SVG.R")

Figure3_A1 = read_rds("../Paper/Figures/Figure_3/Input_mRNAseq_All_GO_up.RDS")
Figure3_A1 = Figure3_A1+
  theme(plot.margin = margin(0,0,0,0))+
  ggtitle("Bulk mRNAseq: GO terms of upregulated genes")

Figure3_A2 = read_rds("../Paper/Figures/Figure_3/Input_mRNAseq_All_GO_down.RDS")
Figure3_A2 = Figure3_A1+
  theme(plot.margin = margin(0,0,0,0))+
  ggtitle("Bulk mRNAseq: GO terms of downregulated genes")

Figure3 = multi_panel_figure(
  width = c((8.5-(8))/2, 8, (8.5-(8))/2),
  height = c((11-2*(5.3+0.025))/2, 5.3, 5.3, (11-2*(5.3+0.025))/2), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "Figure3")
Figure3

Figure3 = Figure3 %>% 
  fill_panel(Figure3_A1, label = "A", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 2, column = 2) %>% 
  fill_panel(Figure3_A2, label = "A'", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 3, column = 2)
Figure3

ggsave(filename = "Figure3.pdf", plot = Figure3, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
