data.seq = readRDS("preprocessed_seq_data.RDS")
gene_violin = function(genes_by_GO="GO_term_selection", GO_term=NA, gene_of_interest=NA, normalization="each_gene"){
  if(genes_by_GO=="GO_term_selection"){
    GO_Term_to_FBID = read_rds("Preprocessed_data/GO_Term_to_FBID.rds")
    # method to take go term and make list of genes
    selected_GO_id = GO_term_tib$GOID[GO_term_tib$description == GO_term]
    FBIDs_in_GO_id = GO_Term_to_FBID$ensembl_id[GO_Term_to_FBID$go_id == selected_GO_id]
    selected_gene_data = 
      data.seq %>% 
      filter(FBGN %in% FBIDs_in_GO_id) %>%
      dplyr::select(1:5) %>% 
      pivot_longer(cols = -FBGN, names_to = "Genotype", values_to = "Mean_TPM")
    if(normalization == "each_gene"){
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::group_by(FBGN) %>% 
        dplyr::mutate(Norm_TPM = (Mean_TPM+1)/(Mean_TPM[Genotype=="MeanTPM_TKV_input"]+1))
    } else if(normalization == "mean_gene"){
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(Norm_TPM = (Mean_TPM+1)/(mean(Mean_TPM[Genotype=="MeanTPM_TKV_input"])+1))
    }

  }else if(genes_by_GO=="Custom_selection"){
    gene_of_interest_tokens = unique(unlist(tokens(gene_of_interest, remove_punct = TRUE)))
    selected_gene_data = 
      data.seq %>% 
      filter(FBGN %in% gene_of_interest_tokens) %>%
      dplyr::select(1:5) %>% 
      pivot_longer(cols = -FBGN, names_to = "Genotype", values_to = "Mean_TPM")
    if(normalization == "each_gene"){
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::group_by(FBGN) %>% 
        dplyr::mutate(Norm_TPM = (Mean_TPM+1)/(Mean_TPM[Genotype=="MeanTPM_TKV_input"]+1))
    } else if(normalization == "mean_gene"){
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(Norm_TPM = (Mean_TPM+1)/(mean(Mean_TPM[Genotype=="MeanTPM_TKV_input"])+1))
    }
  }
  
  selected_gene_data_norm$Genotype = factor(x = selected_gene_data$Genotype, 
                                            levels = c("MeanTPM_TKV_input", 
                                                       "MeanTPM_BamRNAi_input", 
                                                       "MeanTPM_BamHSbam_input", 
                                                       "MeanTPM_youngWT_input"))
  
    gene_violin_plot = ggplot(data = selected_gene_data_norm, mapping = aes(x = Genotype, y = log2(Norm_TPM)))+
      geom_violin()+
      # stat_summary(mapping = aes(group = Genotype), fun.y = median, geom = "line")
      geom_point(position = position_jitter())
      # geom_line(mapping = aes(group = FBGN))
    return(gene_violin_plot)
}
