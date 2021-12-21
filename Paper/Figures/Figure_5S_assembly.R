
if (is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
  setwd("ShinyExpresionMap")
}else if (strsplit(getwd(), "Developmental-Landscape")[[1]][2] == "/ShinyExpresionMap") {
  warning("WD already set to /ShinyExpresionMap")
}else{
  errorCondition("WD is invalid")
}

# FigureS5_A1 = read_rds("../Paper/Figures/Figure_4/Polysome_mRNAseq_GO_BP_down.RDS")

FigureS5 = multi_panel_figure(
  width = c((8.5-(8))/2, 8, (8.5-(8))/2),
  height = c((11-2*(5.3+0.025))/2, 5.3, 5.3, (11-2*(5.3+0.025))/2),
  row_spacing = 0.025, column_spacing = 0, unit = "in",
  panel_label_type = "none", figure_name = "FigureS5")
FigureS5
# 
# FigureS5 = FigureS5 %>% 
#   fill_panel(FigureS5_A1, label = "A", 
#              label_just = "bottom", scaling = "none", panel_clip = "off", row = 2, column = 2)
# FigureS5

ggsave(filename = "Supplemental_Figure5.pdf", plot = FigureS5, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)