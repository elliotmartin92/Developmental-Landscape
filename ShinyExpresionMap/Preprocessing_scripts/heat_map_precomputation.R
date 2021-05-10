modls = function(x){log2(x+1)}

DE_heatmap = function(data_set_to_plot="Input_seq"){
  if(data_set_to_plot == "Input_seq"){
    data.seq = readRDS("ShinyExpresionMap/Preprocessed_data/preprocessed_RNA_seq_data.RDS")
    changing_genes = readRDS("ShinyExpresionMap/Preprocessed_data/developmentally_regulated_gene_list.RDS")
    heat_data = data.seq %>%
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
    rownames(heat_data) = data.seq %>% filter(FBGN %in% changing_genes) %>% pull(FBGN)
    
  }else if (data_set_to_plot == "Polysome_seq"){
    data.seq = readRDS("ShinyExpresionMap/Preprocessed_data/preprocessed_polysome_seq_data.RDS")
    changing_genes = readRDS("ShinyExpresionMap/Preprocessed_data/developmentally_regulated_gene_list_polysome.RDS")
    heat_data = data.seq %>%
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
    rownames(heat_data) = data.seq %>% filter(FBGN %in% changing_genes) %>% pull(FBGN)
    
  }else if (data_set_to_plot == "Single_cell_seq_germline"){
    heat_data = readRDS("ShinyExpresionMap/Preprocessed_data/single_cell_seq_regulated_gene_fold_change_list_GC.RDS")
    
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
  }
  else if (data_set_to_plot == "Single_cell_seq_soma"){
    heat_data = readRDS("ShinyExpresionMap/Preprocessed_data/single_cell_seq_regulated_gene_fold_change_list_germarium_soma.RDS")
    
    column_labels =
      c("TF/CC", 
        "aEc", 
        "cEc", 
        "pEc",
        "FSC/pre-FC",
        "pre-stalk",
        "stalk",
        "polar")
  }
  
  heat_map_plotly = heatmaply(heat_data,
                                showticklabels = c(TRUE, FALSE),
                                labCol=column_labels,
                                seriate = "none",
                                Colv = FALSE)
  write_rds(x = heat_map_plotly, file = paste0("ShinyExpresionMap/Preprocessed_data/", data_set_to_plot, "_plotly_heatmap.RDS"))
}

data_sets = c("Input_seq", "Polysome_seq", "Single_cell_seq_germline", "Single_cell_seq_soma")
lapply(data_sets, DE_heatmap)