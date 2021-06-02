library(tidyverse)
library(ggplot2)
library(stringr)
library(gridExtra)
library(grid)
source("ShinyExpresionMap/server_modules/ggplotWhiteTheme.R")

GO_plot_from_panther = function(all_GO_file_names, plot_title){
  
  GO_file_read_and_clean = function(GO_file_name){
    GO_file_basename = sub('\\.txt$', '', GO_file_name)
    GO_file_comparision = str_replace(GO_file_basename, c("_BP|_MF|_CC"), "")
    GO_file_direction = str_replace(GO_file_comparision, c("up_|down_"), "")
    GO_title = str_replace_all(GO_file_direction, "_", " ")
    GO_file = read_tsv(file = paste0("DE_Analysis/input_go_term/", GO_file_name), skip = 11)
    GO_terms = unlist(strsplit(x = GO_file[[1]], split = " .GO.*"))
    GO_file_cleaning = tibble(GO_term = GO_terms, 
                              FDR = as.numeric(GO_file[[8]]), 
                              FE = as.numeric(GO_file[[6]]), 
                              index = as.numeric(row.names(GO_file)))
    
    # Order terms FDR and wrap strings
    GO_file_cleaning$comparison = str_replace(GO_title, c(" vs "), "\nvs\n")
    # GO_file_cleaning$comparison = str_wrap(GO_title_vs, width = 12)
    GO_file_cleaning$labels = str_wrap(GO_file_cleaning$GO_term, width = 40)
    GO_file_cleaning$labels = factor(GO_file_cleaning$labels, levels = GO_file_cleaning$labels[order(GO_file_cleaning$FDR)])
    
    GO_file_top = 
      GO_file_cleaning %>% 
      filter(FE > 1)
      # arrange(-FDR) %>% 
      # top_n(-5) %>% 
      # mutate(labels = factor(labels, labels))
    return(GO_file_top)
  }
  
  all_cleaned_GOs = lapply(all_GO_file_names, GO_file_read_and_clean)
  all_cleaned_GOs_df = all_cleaned_GOs %>% bind_rows(.id = "GO_term")
  all_cleaned_GOs_df$comparison = factor(all_cleaned_GOs_df$comparison, levels=unique(all_cleaned_GOs_df$comparison))
  top_cleaned_GOs_df = all_cleaned_GOs_df %>% arrange(-FDR) %>% group_by(comparison) %>% top_n(-5)
  
  GO_plot = 
    ggplot(data = top_cleaned_GOs_df)+
    geom_tile(aes(x=comparison, y=labels, fill=-log10(FDR)))+
    labs(fill = expression(paste(-log[10],~'(FDR)')))+
    ggtitle(plot_title)+
    ylab("")+
    xlab("")+
    theme_white()+
    theme(plot.title = element_text(size = 12, margin=margin(0,0,5,0)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))
  
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

files_to_plot = list.files(path = "DE_Analysis/input_go_term/", pattern = "*txt$", recursive = FALSE) #find files with correct ending
files_to_plot_order = data.frame(files_to_plot, index=0)
files_to_plot_order$files_to_plot
files_to_plot_order$type = ff(files_to_plot_order$files_to_plot, c("TKV", "BamRNAi", "BamHSbam"))
files_to_plot_ordered = 
  files_to_plot_order %>% 
  mutate(type = fct_relevel(type, c("TKV", "BamRNAi", "BamHSbam"))) %>%
  arrange(type)
files_to_plot_BP = files_to_plot_ordered$files_to_plot[str_detect(files_to_plot_ordered$files_to_plot, "_BP")]
files_to_plot_BP_Up = files_to_plot_ordered$files_to_plot[str_detect(files_to_plot_ordered$files_to_plot, "up_")]
files_to_plot_BP_Down = files_to_plot_ordered$files_to_plot[str_detect(files_to_plot_ordered$files_to_plot, "down_")]

BP_Up = GO_plot_from_panther(files_to_plot_BP_Up, "BP GO terms of upregulated genes")
BP_Down = GO_plot_from_panther(files_to_plot_BP_Down, "BP GO terms of upregulated genes")

saveRDS(BP_Up, "Paper/Figures/Figure_2/Input_mRNAseq_GO_BP_up.RDS")
saveRDS(BP_Down, "Paper/Figures/Figure_2/Input_mRNAseq_GO_BP_down.RDS")
