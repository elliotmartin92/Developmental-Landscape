
if (is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
  setwd("ShinyExpresionMap/")
}else if (!is.na(strsplit(getwd(), "Developmental-Landscape")[[1]][2])) {
}else{
  errorCondition("WD is invalid")
}

library(ggplot2)
library(multipanelfigure)
library(pdftools)
library(extrafont)
library(tidyverse)
library(gridExtra)
source("../Paper/Helper_functions/image_panel.R")
source("../Paper/Helper_functions/png_as_gg.R")
source("../Paper/Helper_functions/export_plotly2SVG.R")
# font_import(paths = c("C:/Users/Elliot/AppData/Local/Microsoft/Windows/Fonts/"), prompt = F)
source("server_modules/ovary_map_cartoon.R")

Figure1_A1 =  ovary_map_cartoon(text_scale = 12/ggplot2::.pt)+
  theme(aspect.ratio = .28,
        plot.margin = margin(0.0, 0.0, 0.1, 0.0, unit = "in"))
Figure1_A1

Figure1_B1 = png_as_gg("../Paper/Figures/Figure_1/screenshot_1.png")

table_raw = tibble("Sample"=c("UAS-tkv", "bam RNAi", "bam RNAi; HS-bam", "Young-WT"), 
                   "Enriched cell type"=c("GSCs", "CBs", "Cysts", "All early stages"))

Table_1C = qplot(1:10, 1:10, geom = "blank") + 
  theme_void() +
  annotation_custom(
    grob = tableGrob(table_raw, rows = NULL), 
    xmin = 0, xmax = 10, ymin = 0, ymax = 10
  )
Table_1C
  
Figure1 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.0, rep(2.0694, 2), 2.0, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c((11-8*(1.1837+0.025))/2-.2, 1.4837, rep(1.1837, 4), .2837, 1.3837, 1.1837, 1.0, (11-8*(1.1837+0.025))/2-.2), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "Figure1")
Figure1

Figure1 = Figure1 %>% 
  fill_panel(Figure1_A1, label = "A", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 2:3, column = 2:7) %>% 
  fill_panel(Figure1_B1, label = "B", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 4:6, column = 3:5) %>% 
  fill_panel(Table_1C, label = "C", 
             label_just = "top", scaling = "none", panel_clip = "off", row = 8, column = 3:4)
Figure1
  
ggsave(filename = "Figure1.pdf", plot = Figure1, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
