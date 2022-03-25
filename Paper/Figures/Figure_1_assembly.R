
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

Genotypes = c(expression(paste(italic('nosGAL4'), ">UAS-", italic('tkv'))),
              expression(paste(italic('nosGAL4'), ">", italic('bam'), " RNAi")),
              expression(paste(italic('nosGAL4'), ">", italic('bam'), " RNAi; hs-", italic("bam"))),
              expression(paste(italic('nosGAL4'), " (Young)")))
              # expression(paste("Young-WT (", italic('nosGAL4'), ")")))

Enriched_cell_type = c("GSCs", "CBs", "Cysts", "Germarium and\nearly egg chambers")

tg_geno = tableGrob(d = Genotypes, cols = "Genotype")
tg_ct = tableGrob(d = Enriched_cell_type, cols = "Enriched cell type")
table_grob = gtable_cbind(tg_geno, tg_ct)

Table_1B = qplot(1:10, 1:10, geom = "blank") + 
  theme_void() +
  annotation_custom(
    grob = table_grob, 
    xmin = 0, xmax = 10, ymin = 0, ymax = 10
  )
Table_1B

Figure1_C1 = png_as_gg("../Paper/Figures/Figure_1/screenshot_1.png")

  
Figure1 = multi_panel_figure(
  width = c((8.5-4*(2.0694+0.025))/2, 0.0694, 2.0, 2.0694, 1.1347, 0.9347, 2.0, 0.0694, (8.5-4*(2.0694+0.025))/2),
  height = c((11-8*(1.1837+0.025))/2-.2, 1.4837, 1.1837, 1.3837, .4837, rep(1.1837, 3), 1.1837, 0.8, (11-8*(1.1837+0.025))/2-.2), 
  row_spacing = 0.025, column_spacing = 0, unit = "in", 
  panel_label_type = "none", figure_name = "Figure1")
Figure1

Figure1 = Figure1 %>% 
  fill_panel(Figure1_A1, label = "A", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 2:3, column = 2:8) %>% 
  fill_panel(Table_1B, label = "B", 
             label_just = "top", scaling = "none", panel_clip = "off", row = 4, column = 3:5) %>% 
  fill_panel(Figure1_C1, label = "C", 
             label_just = "bottom", scaling = "none", panel_clip = "off", row = 6:9, column = 3:6)
Figure1
  
ggsave(filename = "Figure1.pdf", plot = Figure1, path = "../Paper/Figures/", width = 8.5, height = 11, device = cairo_pdf)
