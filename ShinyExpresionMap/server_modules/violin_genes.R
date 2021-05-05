data.seq = readRDS("Preprocessed_data/preprocessed_RNA_seq_data.RDS")

gene_violin = function(data_set_to_plot="Input_seq", 
                       genes_by_GO="GO_term_selection", 
                       GO_term=NA, gene_of_interest=NA, 
                       normalization="each_gene"){
  
  if(data_set_to_plot=="Input_seq"){
    data.seq = readRDS("Preprocessed_data/preprocessed_RNA_seq_data.RDS")
    data.seq_pared = data.seq[1:5] #extract columns used for plotting
    column_names = c("FBGN",
                     "UAS-tkv", 
                     "bam RNAi", 
                     "bam RNAi; HS-bam", 
                     "Young WT")
    genotype_levels = c("UAS-tkv", 
                        "bam RNAi", 
                        "bam RNAi; HS-bam", 
                        "Young WT")
    
  }else if(data_set_to_plot=="Polysome_seq"){
    data.seq = readRDS("Preprocessed_data/preprocessed_polysome_seq_data.RDS")
    data.seq_pared = data.seq[c(1, 3:6)] #extract columns used for plotting
    column_names = c("FBGN",
                     "UAS-tkv", 
                     "bam RNAi", 
                     "bam RNAi; HS-bam", 
                     "Young WT")
    genotype_levels = c("UAS-tkv", 
                        "bam RNAi", 
                        "bam RNAi; HS-bam", 
                        "Young WT")
    
  }else if (data_set_to_plot == "Single_cell_seq_germline"){
    data.seq = readRDS("Preprocessed_data/preprocessed_single_cell_seq_data.RDS")
    data.seq_pared = data.seq[c(20, 2:10)] #extract columns used for plotting
    column_names = c("FBGN",
                     "GSC CB 2CC", 
                     "4CC", 
                     "8CC", 
                     "16CC",
                     "16CC 2a 1",
                     "16CC 2a 2",
                     "16CC 2b",
                     "16CC 3",
                     "St2")
    genotype_levels = c("GSC CB 2CC", 
                        "4CC", 
                        "8CC", 
                        "16CC",
                        "16CC 2a 1",
                        "16CC 2a 2",
                        "16CC 2b",
                        "16CC 3",
                        "St2")
    }
  
  #select either by GO term or with a custom list of FBGNs
  if(genes_by_GO=="GO_term_selection"){
    GO_Term_to_FBID = read_rds("Preprocessed_data/GO_Term_to_FBID.rds")
    # method to take go term and make list of gen
    selected_GO_id = GO_term_tib$GOID[GO_term_tib$description == GO_term] #Globally declared map of ID to description
    FBIDs_in_GO_id = GO_Term_to_FBID$ensembl_id[GO_Term_to_FBID$go_id == selected_GO_id]
    selected_gene_data = 
      data.seq_pared %>% 
      set_names(column_names) %>% 
      filter(FBGN %in% FBIDs_in_GO_id) %>%
      pivot_longer(cols = -FBGN, names_to = "Genotype", values_to = "Mean_expression")
    # Use custom gene list
  }else if(genes_by_GO=="Custom_selection"){
    # tokenize input to get around variable seps from user
    gene_of_interest_tokens = unique(unlist(tokens(gene_of_interest, remove_punct = TRUE)))
    selected_gene_data = 
      data.seq_pared %>% 
      set_names(column_names) %>%
      filter(FBGN %in% gene_of_interest_tokens) %>%
      pivot_longer(cols = -FBGN, names_to = "Genotype", values_to = "Mean_expression")
  }
  # normalize each gene to the value in UAS-tkv 
  # (also +1 to num and denom to prevent div zero errors and Infs from log)
  # type of normalization changes y-axis label here
  if(data_set_to_plot=="Input_seq"){
    if(normalization == "each_gene"){
      yaxis_label = expression("log"[2]*"(UAS-TKV Normalized TPM)")
      selected_gene_data_norm = 
        selected_gene_data %>% 
        dplyr::group_by(FBGN) %>% 
        dplyr::mutate(Norm_expression = log2((Mean_expression+1)/(Mean_expression[Genotype=="UAS-tkv"]+1)))
      stats = selected_gene_data_norm %>% 
        group_by(Genotype) %>%
        rstatix::t_test(formula = Norm_expression~0, mu=0, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", dodge = 0.8) %>% 
        mutate(Genotype = factor(Genotype, genotype_levels)) %>%   # Reorder stats to match data
        arrange(Genotype) %>% 
        select(-c("x", "xmin", "xmax")) %>%           #workaround to apparent bug with add_xy_position order
        bind_cols("x" = 1:length(genotype_levels),
                  "xmin" = 1:length(genotype_levels),
                  "xmax" = 1:length(genotype_levels))
        
    } else if(normalization == "unNorm"){
      yaxis_label = expression("log"[2]*"(TPM+1)")
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(Norm_expression = log2(Mean_expression+1))
      stats = selected_gene_data_norm %>% 
        ungroup() %>% 
        wilcox_test(formula = Norm_expression~Genotype, paired = TRUE, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", dodge = 0.8, step.increase = 0.5)
    }
  }else if(data_set_to_plot=="Polysome_seq"){
    if(normalization == "each_gene"){
      yaxis_label = expression("log"[2]*"(UAS-TKV Normalized TE)")
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::group_by(FBGN) %>% 
        dplyr::mutate(Norm_expression = log2((Mean_expression)/(Mean_expression[Genotype=="UAS-tkv"])))
      stats = selected_gene_data_norm %>% 
        group_by(Genotype) %>% 
        rstatix::t_test(formula = Norm_expression~0, mu=0, p.adjust.method = "holm") %>% 
        add_xy_position(x = "Genotype", dodge = 0.8) %>% 
       mutate(Genotype = factor(Genotype, genotype_levels)) %>%   # Reorder stats to match data
        arrange(Genotype) %>% 
        select(-c("x", "xmin", "xmax")) %>%           #workaround to apparent bug with add_xy_position order
        bind_cols("x" = 1:length(genotype_levels),
                  "xmin" = 1:length(genotype_levels),
                  "xmax" = 1:length(genotype_levels))
      
    }else if(normalization == "unNorm"){
      yaxis_label = expression("log"[2]*"(TE)")
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(Norm_expression = log2(Mean_expression))
      
      stats = selected_gene_data_norm %>% 
        ungroup() %>% 
        wilcox_test(formula = Norm_expression~Genotype, paired = TRUE, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", dodge = 0.8, step.increase = 0.5)
    }
  }else if(data_set_to_plot=="Single_cell_seq_germline"){
    if(normalization == "each_gene"){
      yaxis_label = expression("log normalized expression to GSC/CB/2CC")
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::group_by(FBGN) %>% 
        dplyr::mutate(Norm_expression = Mean_expression-Mean_expression[Genotype=="GSC CB 2CC"])
      
      stats = selected_gene_data_norm %>% 
        group_by(Genotype) %>% 
        rstatix::t_test(formula = Norm_expression~0, mu=0, p.adjust.method = "holm") %>% #bug here
        add_xy_position(x = "Genotype", dodge = 0.8) %>% 
        mutate(Genotype = factor(Genotype, genotype_levels)) %>%   # Reorder stats to match data
        arrange(Genotype) %>% 
        select(-c("x", "xmin", "xmax")) %>%           #workaround to apparent bug with add_xy_position order
        bind_cols("x" = 1:length(genotype_levels),
                  "xmin" = 1:length(genotype_levels),
                  "xmax" = 1:length(genotype_levels))
      
    }else if(normalization == "unNorm"){
      yaxis_label = expression("log normalized expression")
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(Norm_expression = Mean_expression)
      
      stats = selected_gene_data_norm %>% 
        ungroup() %>% 
        wilcox_test(formula = Norm_expression~Genotype, paired = TRUE, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", dodge = 0.8, step.increase = 0.5)
    }
  }
  # order factors
  selected_gene_data_norm$Genotype = factor(x = selected_gene_data$Genotype, 
                                            levels = genotype_levels)
  # violin plot
    gene_violin_plot = 
      ggplot(data = selected_gene_data_norm, mapping = aes(x = Genotype, y = Norm_expression))+
      geom_violin()+
      stat_pvalue_manual(stats)+
      # stat_compare_means()+
      ylab(yaxis_label)+
      # scale_x_discrete(labels=c("UAS-tkv", 
      #        "bam RNAi", 
      #        "bam RNAi; HS-bam", 
      #        "Young WT"))+
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
      stat_summary(mapping = aes(group = Genotype), 
                   fun = median, fun.min = median, fun.max = median,
                   geom = "crossbar", width = 0.4)+
      geom_point(position = position_jitter(seed = 1, width = 0.2), color="grey60")+
      theme_white()
      # geom_line(mapping = aes(group = FBGN))
    return(gene_violin_plot)
}
