library(tidyverse)
library(openxlsx)
library(rstatix)
library(scales)

fiji_output = read.xlsx("Paper/Figures/Figure_4/Ord_GFP_in_situ_quant.xlsx")

fiji_output_normalized = 
  fiji_output %>% 
  dplyr::group_by(Rep, Staining) %>% 
  dplyr::mutate(proximal_niche_mean_norm = rescale(Mean, to = c(0.1, 1))) %>% 
  dplyr::mutate(proximal_niche_mean_percent_area = rescale(Percent.Area, to = c(0.1, 1)))

# Maybe percent area isn't the best, or thresholding is wrong. Mean seems to work better. Requant to get proper mean for mRNA

translation_efficiency = 
  fiji_output_normalized %>% 
  group_by(Rep, Cell) %>% 
  mutate(Protein_mRNA_ratio = proximal_niche_mean_norm[Staining=="protein"]/proximal_niche_mean_percent_area[Staining=="mRNA"]) %>% 
  filter(Staining!="mRNA")

ggplot(translation_efficiency, aes(x = X, y = Protein_mRNA_ratio))+
  geom_point()
  # geom_smooth(method='loess', formula= y~x)

fiji_output_normalized %>% filter(Staining=="mRNA") %>% 
ggplot(aes(x = X, y = proximal_niche_mean_percent_area))+
  geom_point()
  # ylim(c(0, 1))

fiji_output_normalized %>% filter(Staining=="protein") %>% 
  ggplot(aes(x = X, y = proximal_niche_mean_norm))+
  ylab("Ord protein expression (A.U.)")+
  xlab("Distance from niche (micron)")+
  geom_point()+
  ylim(c(0, 1.2))
