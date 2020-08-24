data.seq = readRDS("preprocessed_seq_data.RDS")
gene_violin = function(genes_by_GO=FALSE, gene_of_interest=NA){
  if(genes_by_GO==TRUE){
    # method to take go term and make list of genes
    selected_gene_data = 
      data.seq %>% 
      filter(FBGN %in% gene_of_interest) %>%
      dplyr::select(1:5) %>% 
      pivot_longer(cols = -FBGN, names_to = "Genotype", values_to = "Mean_TPM")
  }else{
    selected_gene_data = 
      data.seq %>% 
      filter(FBGN %in% gene_of_interest) %>%
      dplyr::select(1:5) %>% 
      pivot_longer(cols = -FBGN, names_to = "Genotype", values_to = "Mean_TPM")
  }
    ggplot(data = selected_gene_data, mapping = aes(x = Genotype, y = log2(Mean_TPM+1)))+
    geom_violin()+
    geom_point()+
    geom_line(mapping = aes(group = FBGN))
}

test_violin = gene_violin(gene_of_interest = c("FBgn0000003", "FBgn0000008", "FBgn0000018"))
