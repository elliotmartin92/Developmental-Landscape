library(tidyverse)
library(ggplot2)
library(stringr)
library(gridExtra)
library(grid)
source("ShinyExpresionMap/server_modules/ggplotWhiteTheme.R")

GO_plot_from_panther = function(GO_file_name){
  GO_file_basename = sub('\\.txt$', '', GO_file_name)
  GO_title = str_replace_all(GO_file_basename, "_", " ")
  GO_file = read_tsv(file = paste0("DE_Analysis/input_go_term/", GO_file_name), skip = 11)
  GO_terms = unlist(strsplit(x = GO_file[[1]], split = " .GO.*"))
  GO_file_cleaning = tibble(GO_term = GO_terms, 
                            FDR = GO_file[[8]], 
                            FE = GO_file[[6]], 
                            index = as.numeric(row.names(GO_file)))
  
  # Order terms FDR and wrap strings
  GO_file_cleaning$labels = str_wrap(GO_file_cleaning$GO_term, width = 35)
  GO_file_cleaning$labels = factor(GO_file_cleaning$labels, levels = GO_file_cleaning$labels[order(GO_file_cleaning$FDR)])
  
  GO_file_top = 
    GO_file_cleaning %>% 
    filter(FE > 1) %>% 
    arrange(-FDR) %>% 
    top_n(-8) %>% 
    mutate(labels = factor(labels, labels))
  
  GO_plot = 
  ggplot(data = GO_file_top)+
    geom_bar(aes(x=labels, y=-log10(FDR)), stat = "identity")+
    ggtitle(GO_title)+
    xlab("")+
    ylab(expression(paste("-", log[10], "(FDR)")))+
    scale_y_continuous(expand = expansion(mult = c(0,0.1)))+
    theme_white()+
    theme(legend.position = "", 
          plot.title = element_text(size=12))+
    coord_flip()
  
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
GO_plots_BP = lapply(files_to_plot_BP, GO_plot_from_panther)
all_plots_bp = grid.arrange(grobs = GO_plots_BP)
ggsave(filename = "DE_Analysis/input_go_term/all_BP_plots.pdf", plot = all_plots_bp, width = 20, height = 14)

files_to_plot_MF = files_to_plot_ordered$files_to_plot[str_detect(files_to_plot_ordered$files_to_plot, "_MF")]
GO_plots_MF = lapply(files_to_plot_BP, GO_plot_from_panther)
all_plots_MF = grid.arrange(grobs = GO_plots_MF)
ggsave(filename = "DE_Analysis/input_go_term/all_MF_plots.pdf", plot = all_plots_bp, width = 20, height = 14)

files_to_plot_CC = files_to_plot_ordered$files_to_plot[str_detect(files_to_plot_ordered$files_to_plot, "_CC")]
GO_plots_CC = lapply(files_to_plot_CC, GO_plot_from_panther)
all_plots_CC = grid.arrange(grobs = GO_plots_CC)
ggsave(filename = "DE_Analysis/input_go_term/all_CC_plots.pdf", plot = all_plots_CC, width = 20, height = 14)
