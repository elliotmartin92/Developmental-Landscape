data.seq = readRDS("preprocessed_seq_data.RDS")
gene_violin = function(genes_by_GO="GO_term_selection", GO_term=NA, gene_of_interest=NA, normalization="each_gene"){
  #select either by GO term or with a custom list of FBGNs
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
    # Use custom gene list
  }else if(genes_by_GO=="Custom_selection"){
    # tokenize input to get around variable seps from user
    gene_of_interest_tokens = unique(unlist(tokens(gene_of_interest, remove_punct = TRUE)))
    selected_gene_data = 
      data.seq %>% 
      filter(FBGN %in% gene_of_interest_tokens) %>%
      dplyr::select(1:5) %>% 
      pivot_longer(cols = -FBGN, names_to = "Genotype", values_to = "Mean_TPM")
  }
  # normalize each gene to the value in UAS-tkv 
  # (also +1 to num and denom to prevent div zero errors and Infs from log)
  # type of normalization changes y-axis label here
  if(normalization == "each_gene"){
    yaxis_label = expression("log"[2]*"(UAS-TKV Normalized TPM)")
    selected_gene_data_norm = selected_gene_data %>% 
      dplyr::group_by(FBGN) %>% 
      dplyr::mutate(Norm_TPM = log2((Mean_TPM+1)/(Mean_TPM[Genotype=="MeanTPM_TKV_input"]+1)))
  } else if(normalization == "unNorm"){
    yaxis_label = expression("log"[2]*"(TPM+1)")
    selected_gene_data_norm = selected_gene_data %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(Norm_TPM = log2(Mean_TPM+1))
  }
  # order factors
  selected_gene_data_norm$Genotype = factor(x = selected_gene_data$Genotype, 
                                            levels = c("MeanTPM_TKV_input", 
                                                       "MeanTPM_BamRNAi_input", 
                                                       "MeanTPM_BamHSbam_input", 
                                                       "MeanTPM_youngWT_input"))
  # violin plot
    gene_violin_plot = 
      ggplot(data = selected_gene_data_norm, mapping = aes(x = Genotype, y = Norm_TPM))+
      geom_violin()+
      ylab(yaxis_label)+
      scale_x_discrete(labels=c("UAS-tkv", 
             "bam RNAi", 
             "bam RNAi; HS-bam", 
             "Young WT"))+
      stat_summary(mapping = aes(group = Genotype), 
                   fun = median, fun.min = median, fun.max = median,
                   geom = "crossbar", width = 0.4)+
      geom_point(position = position_jitter(seed = 1, width = 0.2), color="grey60")+
      theme_white()
      # geom_line(mapping = aes(group = FBGN))
    return(gene_violin_plot)
}
