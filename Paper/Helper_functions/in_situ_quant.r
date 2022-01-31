library(tidyverse)
library(openxlsx)
library(rstatix)
library(scales)
library(here)

plot_insitu_quant = function(staining_to_plot, xlsx_file, gene_name) {
  fiji_output = read.xlsx(xlsx_file, sheet = 1)
  
  fiji_output_normalized =
    fiji_output %>%
    dplyr::group_by(Germarium, Staining) %>%
    dplyr::mutate(norm_to_1 = Mean/max(Mean))
    # dplyr::mutate(proximal_niche_mean_norm = rescale(Mean, to = c(0.1, 1)))
  
if(staining_to_plot == "mRNA")
  fiji_output_normalized %>% 
    filter(Staining==staining_to_plot) %>% 
  ggplot(aes(x = X, y = norm_to_1))+
    geom_smooth(method='loess', formula= y~x)+
    ylab(TeX(paste("$\\textit{", gene_name, "}\\, mRNA\\, expression\\, (A.U.)$")))+
    xlab("Distance from niche (micron)")+
    geom_point()+
    theme_white()+
    theme(aspect.ratio = 0.5, 
          axis.text.x = element_text(size=10),
          plot.margin =  margin(c(.2,.2,.1,.1), unit = "in"))
  
  else if (staining_to_plot == "protein") {
    fiji_output_normalized %>% 
    filter(Staining==staining_to_plot) %>% 
    ggplot(aes(x = X, y = norm_to_1))+
    geom_smooth(method='loess', formula= y~x)+
    ylab(TeX(paste("$", gene_name, "\\, protein\\, expression\\, (A.U.)$")))+
    xlab("Distance from niche (micron)")+
    geom_point()+
    theme_white()+
    theme(aspect.ratio = 0.5, 
          axis.text.x = element_text(size=10),
          plot.margin =  margin(c(.2,.2,.1,.1), unit = "in"))
    
  }else if (staining_to_plot == "TE") {    
    translation_efficiency =
      fiji_output_normalized %>%
      group_by(Germarium, Cell) %>%
      # mutate(Protein_mRNA_ratio = proximal_niche_mean_norm[Staining=="protein"]/
      #          proximal_niche_mean_percent_area[Staining=="mRNA"]) %>%
      mutate(Protein_mRNA_Mean_ratio = Mean[Staining=="protein"]/Mean[Staining=="mRNA"]) %>%
      filter(Staining!="mRNA")
    
      ggplot(translation_efficiency, aes(x = X, y = Protein_mRNA_Mean_ratio))+
      ylab(TeX(paste("$\\textit{", gene_name, "}\\, translation\\, efficiency (mRNA/protein)\\, (A.U.)$")))+
      xlab("Distance from niche (micron)")+
      geom_smooth(method='loess', formula= y~x)+
      geom_point()+
      theme_white()+
      theme(aspect.ratio = 0.5, 
            axis.text.x = element_text(size=10),
            plot.margin =  margin(c(.2,.2,.1,.1), unit = "in"))
      
  }else{errorCondition("staining_to_plot provided should be of mRNA or protein")}
}

# ggsave(filename = here("Paper", "Figures", "Figure_4", "ord_TE_plot.png"), ord_TE_plot, width = 3, height = 3, dpi = 300)
# ggsave(filename = here("Paper", "Figures", "Figure_4", "ord_mRNA_plot.png"), ord_mRNA_plot, width = 3, height = 3, dpi = 300)
# ggsave(filename = here("Paper", "Figures", "Figure_4", "ord_protein_plot.png"), ord_protein_plot, width = 3, height = 3, dpi = 300)
