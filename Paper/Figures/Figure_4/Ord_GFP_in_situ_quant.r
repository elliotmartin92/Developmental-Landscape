library(tidyverse)
library(openxlsx)
library(rstatix)
library(scales)
library(here)

plot_ord_quant = function(staining_to_plot) {
  fiji_output = read.xlsx("../Paper/Figures/Figure_4/Ord_GFP_in_situ_quant.xlsx")
  
  fiji_output_normalized =
    fiji_output %>%
    dplyr::group_by(Rep, Staining) %>%
    dplyr::mutate(proximal_niche_mean_norm = rescale(Mean, to = c(0.1, 1))) %>%
    dplyr::mutate(proximal_niche_mean_percent_area = rescale(Percent.Area, to = c(0.1, 1)))
  
  # Maybe percent area isn't the best, or thresholding is wrong. Mean seems to work better. Requant to get proper mean for mRNA
  
  # translation_efficiency = 
  #   fiji_output_normalized %>% 
  #   group_by(Rep, Cell) %>% 
  #   mutate(Protein_mRNA_ratio = proximal_niche_mean_norm[Staining=="protein"]/proximal_niche_mean_percent_area[Staining=="mRNA"]) %>% 
  #   filter(Staining!="mRNA")
  
  # ord_TE_plot =
  # ggplot(translation_efficiency, aes(x = X, y = Protein_mRNA_ratio))+
  #   ylab("Ord translation efficiency (mRNA/protein) (A.U.)")+
  #   xlab("Distance from niche (micron)")+
  #   geom_point()+
  #   theme_white()
  #   # geom_smooth(method='loess', formula= y~x)
  
if(staining_to_plot == "mRNA")
  fiji_output_normalized %>% filter(Staining==staining_to_plot) %>% 
  ggplot(aes(x = X, y = Mean))+
    ylab("Ord mRNA expression (A.U.)")+
    xlab("Distance from niche (micron)")+
    geom_point()+
    theme_white()+
    theme(aspect.ratio = 0.5)
    # ylim(c(0, 1.2))
  
  else if (staining_to_plot == "protein") {
  fiji_output_normalized %>% 
    filter(Staining==staining_to_plot) %>% 
    ggplot(aes(x = X, y = Mean))+
    ylab("Ord protein expression (A.U.)")+
    xlab("Distance from niche (micron)")+
    geom_point()+
    theme_white()+
    theme(aspect.ratio = 0.5)
    # ylim(c(0, 1.2))
  }else{errorCondition("staining_to_plot provided should be of mRNA or protein")}
}

# ggsave(filename = here("Paper", "Figures", "Figure_4", "ord_TE_plot.png"), ord_TE_plot, width = 3, height = 3, dpi = 300)
# ggsave(filename = here("Paper", "Figures", "Figure_4", "ord_mRNA_plot.png"), ord_mRNA_plot, width = 3, height = 3, dpi = 300)
# ggsave(filename = here("Paper", "Figures", "Figure_4", "ord_protein_plot.png"), ord_protein_plot, width = 3, height = 3, dpi = 300)
