library(tidyverse)
here::i_am("Paper/Figures/Figure_4/median_fold_change_double_strand_break_repair.R")
library(here)

input = read_csv(here("Paper/Figures/Figure_5/Selected_gene_expression_from_Input_seq_of_GO_term_double-strand break repair.csv"))
polysome = read_csv(here("Paper/Figures/Figure_5/Selected_gene_expression_from_Polysome_seq_of_GO_term_double-strand break repair.csv"))
sc_germline = read_csv(here("Paper/Figures/Figure_5/Selected_gene_expression_from_Single_cell_seq_germline_of_GO_term_double-strand break repair.csv"))

polysome %>% group_by(Genotype) %>% 
  mutate(fold_change_vs_TKV = 2^Norm_expression) %>% 
  filter(Genotype == "bam RNAi") %>% 
  summarise(median(fold_change_vs_TKV))

polysome %>% group_by(Genotype) %>% 
  mutate(fold_change_vs_TKV = 2^Norm_expression) %>% 
  filter(Genotype == "bam RNAi; HS-bam") %>% 
  summarise(median(fold_change_vs_TKV))

polysome %>% group_by(Genotype) %>% 
  mutate(fold_change_vs_TKV = 2^Norm_expression) %>% 
  filter(Genotype == "Young WT") %>% 
  summarise(median(fold_change_vs_TKV))


sc_germline %>% group_by(Genotype) %>% 
  mutate(fold_change_vs_TKV = 2^Norm_expression) %>% 
  filter(Genotype == "4CC") %>% 
  summarise(median(fold_change_vs_TKV))

sc_germline %>% group_by(Genotype) %>% 
  mutate(fold_change_vs_TKV = 2^Norm_expression) %>% 
  filter(Genotype == "8CC") %>% 
  summarise(median(fold_change_vs_TKV))
