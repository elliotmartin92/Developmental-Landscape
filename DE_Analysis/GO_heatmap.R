library(tidyverse)
library(ggplot2)
library(stringr)
library(gridExtra)
library(grid)
library(latex2exp)
library(viridis)
source("ShinyExpresionMap/server_modules/ggplotWhiteTheme.R")

GO_plot_from_panther = function(all_GO_file_names, plot_title, n_per_geno=5, return_all_go_list = FALSE){
  
  GO_file_read_and_clean = function(GO_file_name){
    GO_file_basename = basename(GO_file_name)
    GO_file_cleaned_name = sub('\\.txt$', '', GO_file_basename)
    GO_file_comparision = str_replace(GO_file_cleaned_name, c("_BP|_MF|_CC"), "")
    GO_type = str_extract(GO_file_cleaned_name, c("BP|MF|CC"))
    GO_file_direction = str_replace(GO_file_comparision, c("up_|down_"), "")
    GO_title = str_replace_all(GO_file_direction, "_", " ")
    GO_file = read_tsv(file = GO_file_name, skip = 11)
    GO_terms = unlist(strsplit(x = GO_file[[1]], split = " .GO.*"))
    GO_file_cleaning = tibble(GO_term = GO_terms,
                              GO_term_class = GO_type,
                              FDR = as.numeric(GO_file[[8]]), 
                              FE = as.numeric(GO_file[[6]]), 
                              index = as.numeric(row.names(GO_file)))
    
    # Order terms FDR and wrap strings
    GO_file_cleaning$comparison = GO_title
    # GO_file_cleaning$comparison = str_wrap(GO_title_vs, width = 12)
    GO_file_cleaning$labels = str_wrap(GO_file_cleaning$GO_term, width = 40)
    GO_file_cleaning$labels = factor(GO_file_cleaning$labels, levels = GO_file_cleaning$labels[order(GO_file_cleaning$FDR)])
    
    GO_file_top = 
      GO_file_cleaning %>% 
      filter(FE > 1)
    return(GO_file_top)
  }
  
  all_cleaned_GOs = lapply(all_GO_file_names, GO_file_read_and_clean)
  all_cleaned_GOs_df = all_cleaned_GOs %>% bind_rows(.id = "GO_term")
  all_cleaned_GOs_df$comparison = factor(all_cleaned_GOs_df$comparison, levels=unique(all_cleaned_GOs_df$comparison))
  
  if(return_all_go_list == TRUE){
    all_cleaned_GOs_df$comparison = str_replace_all(all_cleaned_GOs_df$comparison, pattern = "\\n", " ")
    return(all_cleaned_GOs_df)
  }
  
  top_cleaned_GOs = 
    all_cleaned_GOs_df %>% 
    arrange(-FDR) %>% 
    group_by(comparison) %>% 
    top_n(-n_per_geno) %>% 
    pull(labels)
  
  top_cleaned_GOs_df = 
    all_cleaned_GOs_df %>% 
    arrange(-FDR) %>% 
    group_by(comparison) %>% 
    filter(labels %in% top_cleaned_GOs)
  
  formatted_labels = c("TKV vs BamHSbam" = 
                         parse(text = TeX(r'($\overset{>UAS-\textit{tkv}\, vs}{> \textit{bam}\, RNAi;\, hs-\textit{bam}}$)')),
                       "TKV vs youngWT" = 
                         parse(text = TeX(r'($\overset{>UAS-\textit{tkv}\, vs}{young\, WT}$)')), 
                       "BamRNAi vs BamHSbam" = 
                         parse(text = TeX(r'($\overset{> \textit{bam} \, RNAi\, vs}{> \textit{bam}\, RNAi;\, hs-\textit{bam}}$)')), 
                       "BamRNAi vs youngWT" = 
                         parse(text = TeX(r'($\overset{> \textit{bam} \, RNAi\, vs}{young\, WT}$)')),  
                       "BamHSbam vs youngWT" = 
                         parse(text = TeX(r'($\overset{> \textit{bam}\, RNAi;\, hs-\textit{bam}}{vs\, young\, WT}$)')))
  
  GO_plot = 
    ggplot(data = top_cleaned_GOs_df)+
    geom_tile(aes(x=comparison, y=labels, fill=-log10(FDR)))+
    labs(fill = TeX(r'($\overset{-log_{10}}{FDR}$)'), parse=TRUE)+
    ggtitle(plot_title)+
    scale_x_discrete(labels = formatted_labels)+
    scale_fill_viridis()+
    ylab("")+
    xlab("")+
    theme_white()+
    theme(plot.title = element_text(size = 12, margin=margin(0,0,5,0)),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8))
  
  # ggsave(paste0("DE_Analysis/input_go_term/", GO_file_basename, "8_bar.pdf"), plot = GO_plot,  height=4.5, width=4.5)
  return(GO_plot)
}

ff = function(x, patterns, replacements = patterns, fill = NA, ...) 
{
  stopifnot(length(patterns) == length(replacements))
  
  ans = rep_len(as.character(fill), length(x))    
  empty = seq_along(x)
  
  for(i in seq_along(patterns)) {
    greps = grepl(patterns[[i]], x[empty], ...)
    ans[empty[greps]] = replacements[[i]]  
    empty = empty[!greps]
  }
  
  return(ans)
}

files_to_plot = list.files(path = "DE_Analysis/input_go_term/", pattern = "*txt$", recursive = FALSE, full.names = TRUE) #find files with correct ending
files_to_plot_order = data.frame(files_to_plot, index=0)
files_to_plot_order$files_to_plot
files_to_plot_order$type = ff(files_to_plot_order$files_to_plot, c("TKV", "BamRNAi", "BamHSbam"))
files_to_plot_ordered = 
  files_to_plot_order %>% 
  mutate(type = fct_relevel(type, c("TKV", "BamRNAi", "BamHSbam"))) %>%
  arrange(type)

files_to_plot_BP = files_to_plot_ordered$files_to_plot[str_detect(files_to_plot_ordered$files_to_plot, "_BP")]
files_to_plot_BP_Up = files_to_plot_BP[str_detect(files_to_plot_BP, "up_")]
files_to_plot_BP_Down = files_to_plot_BP[str_detect(files_to_plot_BP, "down_")]

BP_Up = GO_plot_from_panther(files_to_plot_BP_Up, "BP GO terms of upregulated genes")
BP_Down = GO_plot_from_panther(files_to_plot_BP_Down, "BP GO terms of downregulated genes")

# saveRDS(BP_Up, "Paper/Figures/Figure_3/Input_mRNAseq_GO_BP_up.RDS")
# saveRDS(BP_Down, "Paper/Figures/Figure_3/Input_mRNAseq_GO_BP_down.RDS")

files_to_plot_Up = files_to_plot_ordered$files_to_plot[str_detect(files_to_plot_ordered$files_to_plot, "up_")]
files_to_plot_Down = files_to_plot_ordered$files_to_plot[str_detect(files_to_plot_ordered$files_to_plot, "down_")]

#heatmaps of top 5 GO terms from any category per comparison
all_GO_Up = GO_plot_from_panther(files_to_plot_Up, "GO terms of upregulated genes")
all_GO_Down = GO_plot_from_panther(files_to_plot_Down, "GO terms of downregulated genes")

# saveRDS(all_GO_Up, "Paper/Figures/Figure_3/Input_mRNAseq_All_GO_up.RDS")
# saveRDS(all_GO_Down, "Paper/Figures/Figure_3/Input_mRNAseq_All_GO_down.RDS")

#lists of GO terms from any category per comparison
all_GO_Up_list = GO_plot_from_panther(files_to_plot_Up, "GO terms of upregulated genes", return_all_go_list = TRUE)
all_GO_Down_list = GO_plot_from_panther(files_to_plot_Down, "GO terms of downregulated genes", return_all_go_list = TRUE)

# list_of_datasets <- list("all GO terms_Up" = all_GO_Up_list, "all GO terms Down" = all_GO_Down_list)
# write.xlsx(list_of_datasets, file = "Paper/Figures/Figure_3/Input_mRNAseq_All_GO_terms.xlsx")

files_to_plot = list.files(path = "DE_Analysis/polysome_go_term/", pattern = "*txt$", recursive = FALSE, full.names = TRUE) #find files with correct ending
files_to_plot_order = data.frame(files_to_plot, index=0)
files_to_plot_order$files_to_plot
files_to_plot_order$type = ff(files_to_plot_order$files_to_plot, c("TKV", "BamRNAi", "BamHSbam"))
ordered_genotypes = c("TKV", "BamRNAi", "BamHSbam")
present_genotypes = ordered_genotypes[ordered_genotypes %in% files_to_plot_order$type]
files_to_plot_ordered = 
  files_to_plot_order %>% 
  mutate(type = fct_relevel(type, present_genotypes)) %>%
  arrange(type)
files_to_plot_BP = files_to_plot_ordered$files_to_plot[str_detect(files_to_plot_ordered$files_to_plot, "_BP")]
files_to_plot_BP_Up = files_to_plot_ordered$files_to_plot[str_detect(files_to_plot_ordered$files_to_plot, "up_")]
files_to_plot_BP_Down = files_to_plot_ordered$files_to_plot[str_detect(files_to_plot_ordered$files_to_plot, "down_")]

# BP_Up = GO_plot_from_panther(files_to_plot_BP_Up, "BP GO terms of upregulated genes")
BP_Down = GO_plot_from_panther(files_to_plot_BP_Down, "BP GO terms of downregulated genes", n_per_geno = 10)
# saveRDS(BP_Down, "Paper/Figures/Figure_4/Polysome_mRNAseq_GO_BP_down.RDS")


plot(TeX(r'($\overset{>UAS-\textit{tkv}\,\nvs}{> \textit{bam}\, RNAi;\, hs-\textit{bam}}$)'))
     
