modls = function(x){log2(x+1)}

DE_heatmap = function(data_set_to_plot="Input_seq"){
  if(data_set_to_plot == "Input_seq"){
    data.seq = readRDS("Preprocessed_data/preprocessed_RNA_seq_data.RDS")
    changing_genes = readRDS("Preprocessed_data/developmentally_regulated_gene_list.RDS")
    heat.data = data.seq %>%
      ungroup() %>% 
      filter(FBGN %in% changing_genes) %>%
      dplyr::select(MeanTPM_TKV_input,
                    MeanTPM_BamRNAi_input,
                    MeanTPM_BamHSbam_input,
                    MeanTPM_youngWT_input) %>% 
      modls() %>%
      data.frame()
    
    column_labels =
      c("UAS-tkv", 
        "bam RNAi", 
        "bam RNAi; HS-bam", 
        "Young WT")
    rownames(heat.data) = data.seq %>% filter(FBGN %in% changing_genes) %>% pull(FBGN)
    
  }else if (data_set_to_plot == "Polysome_seq"){
    data.seq = readRDS("Preprocessed_data/preprocessed_polysome_seq_data.RDS")
    changing_genes = readRDS("Preprocessed_data/developmentally_regulated_gene_list_polysome.RDS")
    heat.data = data.seq %>%
      ungroup() %>% 
      filter(FBGN %in% changing_genes) %>%
      dplyr::select(Mean_log2_polysome_over_input_TKV,
                    Mean_log2_polysome_over_input_BamRNAi,
                    Mean_log2_polysome_over_input_BamHSbam,
                    Mean_log2_polysome_over_input_youngWT) %>%
      data.frame()
    
    column_labels =
      c("UAS-tkv", 
        "bam RNAi", 
        "bam RNAi; HS-bam", 
        "Young WT")
    rownames(heat.data) = data.seq %>% filter(FBGN %in% changing_genes) %>% pull(FBGN)
  
  }else if (data_set_to_plot == "Single_cell_seq"){
    data.seq = readRDS("Preprocessed_data/preprocessed_single_cell_seq_data.RDS")
    changing_genes = readRDS("Preprocessed_data/Single_cell_seq_regulated_gene_list.RDS")
    heat.data = data.seq %>% 
      ungroup() %>% 
      filter(symbol %in% changing_genes) %>% 
      select(c(2:10)) %>% 
      modls() %>%
      data.frame()
    
    column_labels =
      c("GSC CB 2CC", 
        "4CC", 
        "8CC", 
        "16CC",
        "16CC 2a 1",
        "16CC 2a 2",
        "16CC 2b",
        "16CC 3",
        "St2")
    rownames(heat.data) = data.seq %>% filter(symbol %in% changing_genes) %>% pull(symbol)
  }
  
  heat_map_global <<- heatmaply(heat.data,
                                showticklabels = c(TRUE, FALSE),
                                labCol=column_labels,
                                seriate = "none",
                                Colv = FALSE)
}