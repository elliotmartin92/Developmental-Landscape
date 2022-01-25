data.seq = readRDS("Preprocessed_data/preprocessed_RNA_seq_data.RDS")
par(lheight=0.2)

# Read list containing all GO terms, declared globally to allow for access in the following function and by the Shiny app (defined scope would be better)
if (!exists("GO_term_tib")) {
  GO_term_tib <<-  read_tsv("Preprocessed_data/all_go_terms.tsv")
  GO_term_description <<- GO_term_tib$description
}

# Function that produces violin plots

gene_violin = function(data_set_to_plot="Input_seq", 
                       genes_by_GO="GO_term_selection", 
                       GO_term=NA, 
                       gene_of_interest=NA, 
                       normalization="each_gene",
                       text_scale,
                       pval_yadj=2){
  
  # Set parameters based on dataset selected
  if(data_set_to_plot=="Input_seq"){
    data.seq = readRDS("Preprocessed_data/preprocessed_RNA_seq_data.RDS")
    data.seq_pared = data.seq[c(1, 10, 2:5)] #extract columns used for plotting
    column_names = c("FBGN",
                     "Symbol",
                     "UAS-tkv", 
                     "bam RNAi", 
                     "bam RNAi; HS-bam", 
                     "Young WT")
    
    genotype_levels = c("UAS-tkv", 
                        "bam RNAi", 
                        "bam RNAi; HS-bam", 
                        "Young WT")
    
    formatted_labels = c("UAS-tkv" = 
                           parse(text = TeX(r'($\overset{> UAS- \textit{tkv}}{(GSCs)}$)')),
                         "bam RNAi" = 
                           parse(text = TeX(r'($\overset{> \textit{bam} \, RNAi}{(CBs)}$)')), 
                         "bam RNAi; HS-bam" = 
                           parse(text = TeX(r'($\overset{> \textit{bam} \, RNAi; \, hs-\textit{bam}}{(Cysts)}$)')), 
                         "Young WT" = 
                           "Young WT")
    
  }else if(data_set_to_plot=="Polysome_seq"){
    data.seq = readRDS("Preprocessed_data/preprocessed_polysome_seq_data.RDS")
    data.seq_pared = data.seq[c(1, 23, 3:6)] #extract columns used for plotting
    column_names = c("FBGN",
                     "Symbol",
                     "UAS-tkv", 
                     "bam RNAi", 
                     "bam RNAi; HS-bam", 
                     "Young WT")
    
    genotype_levels = c("UAS-tkv", 
                        "bam RNAi", 
                        "bam RNAi; HS-bam", 
                        "Young WT")
    
    formatted_labels = c("UAS-tkv" = 
                           parse(text = TeX(r'($\overset{> UAS- \textit{tkv}}{(GSCs)}$)')),
                         "bam RNAi" = 
                           parse(text = TeX(r'($\overset{> \textit{bam} \, RNAi}{(CBs)}$)')), 
                         "bam RNAi; HS-bam" = 
                           parse(text = TeX(r'($\overset{> \textit{bam} \, RNAi; \, hs-\textit{bam}}{(Cysts)}$)')), 
                         "Young WT" = 
                           "Young WT")
    
  }else if (data_set_to_plot == "Single_cell_seq_germline"){
    data.seq = readRDS("Preprocessed_data/preprocessed_single_cell_seq_data_GC.RDS")
    data.seq_pared = data.seq[c(1, 2, 3:11)] #extract columns used for plotting
    column_names = c("FBGN",
                     "Symbol",
                     "GSC/CB/2CC", 
                     "4CC", 
                     "8CC", 
                     "16CC\n2a 1",
                     "16CC\n2a 2",
                     "16CC\n2ab",
                     "16CC\n2b",
                     "16CC\n3",
                     "St2")
    
    genotype_levels = c("GSC/CB/2CC", 
                        "4CC", 
                        "8CC", 
                        "16CC\n2a 1",
                        "16CC\n2a 2",
                        "16CC\n2ab",
                        "16CC\n2b",
                        "16CC\n3",
                        "St2")
    
    formatted_labels = c("GSC/CB/2CC", 
                         "4CC", 
                         "8CC", 
                         "16CC\n2a 1",
                         "16CC\n2a 2",
                         "16CC\n2ab",
                         "16CC\n2b",
                         "16CC\n3",
                         "St2")
    
  }else if (data_set_to_plot == "Single_cell_seq_soma"){
    if (normalization == "each_gene") {
      data.seq = readRDS("Preprocessed_data/preprocessed_single_cell_seq_data_germarium_soma.RDS")
      data.seq_pared = data.seq[c(1, 2, 3:10)] #extract columns used for plotting
    }else if (normalization == "unNorm") {
      data.seq = readRDS("Preprocessed_data/single_cell_seq_fold_changes_germarium_soma.RDS")
      data.seq_pared = data.seq %>% 
        relocate(Symbol, .after = FBGN)
    }
    
    
    column_names = c("FBGN",
                     "Symbol",
                     "TF/CC", 
                     "aEc", 
                     "cEc", 
                     "pEc",
                     "FSC/pre-FC",
                     "pre-stalk",
                     "stalk",
                     "polar")
    
    genotype_levels = c("TF/CC", 
                        "aEc", 
                        "cEc", 
                        "pEc",
                        "FSC/pre-FC",
                        "pre-stalk",
                        "stalk",
                        "polar")
    
    formatted_labels = c("TF/CC", 
                         "aEc", 
                         "cEc", 
                         "pEc",
                         "FSC/pre-FC",
                         "pre-stalk",
                         "stalk",
                         "polar")
  }
  
  # select either by GO term or with a custom list of FBGNs
  if(genes_by_GO=="GO_term_selection"){
    GO_Term_to_FBID = read_rds("Preprocessed_data/GO_Term_to_FBID.rds")
    # method to take go term and make list of genes
    selected_GO_id = GO_term_tib$GOID[GO_term_tib$description == GO_term] #Globally declared map of ID to description
    FBIDs_in_GO_id = GO_Term_to_FBID$ensembl_id[GO_Term_to_FBID$go_id == selected_GO_id]
    selected_gene_data = 
      data.seq_pared %>% 
      set_names(column_names) %>% 
      filter(FBGN %in% FBIDs_in_GO_id) %>%
      pivot_longer(cols = -c(FBGN, Symbol), names_to = "Genotype", values_to = "Mean_expression")
    # Use custom gene list
  }else if(genes_by_GO=="Custom_selection"){
    # tokenize input to get around variable seps from user
    gene_of_interest_tokens = unique(unlist(tokens(gene_of_interest, remove_punct = TRUE)))
    selected_gene_data = 
      data.seq_pared %>% 
      set_names(column_names) %>%
      filter(FBGN %in% gene_of_interest_tokens) %>%
      pivot_longer(cols = -c(FBGN, Symbol), names_to = "Genotype", values_to = "Mean_expression")
  }
  # normalize each gene to the value in UAS-tkv 
  # (also +1 to num and denom to prevent div zero errors and Infs from log)
  # type of normalization changes y-axis label here
  if(data_set_to_plot=="Input_seq"){
    if(normalization == "each_gene"){
      yaxis_label = TeX(r'($\overset{log _2 \,(> UAS- \textit{tkv}}{Normalized \, TPM+1)}$)')
      selected_gene_data_norm = 
        selected_gene_data %>% 
        dplyr::group_by(FBGN, Symbol) %>% 
        dplyr::mutate(Norm_expression = log2((Mean_expression+1)/(Mean_expression[Genotype=="UAS-tkv"]+1)))
      
      selected_gene_data_norm$Genotype = factor(x = selected_gene_data$Genotype, 
                                                levels = genotype_levels)
      
      comparisons = list(c("UAS-tkv", "bam RNAi"), 
                         c("UAS-tkv", "bam RNAi; HS-bam"), 
                         c("UAS-tkv", "Young WT"))
      
      stats = selected_gene_data_norm %>% 
        ungroup() %>% 
        rstatix::t_test(formula = Norm_expression~Genotype, comparisons = comparisons, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", step.increase = 0.5) 
      
    } else if(normalization == "unNorm"){
      yaxis_label = expression("log"[2]*"(TPM+1)")
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(Norm_expression = log2(Mean_expression+1))
      stats = selected_gene_data_norm %>% 
        ungroup() %>% 
        wilcox_test(formula = Norm_expression~Genotype, paired = TRUE, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", dodge = 0.8, step.increase = 0.7)
    }
  }else if(data_set_to_plot=="Polysome_seq"){
    if(normalization == "each_gene"){
      yaxis_label = TeX(r'($\overset{log _2 \,(> UAS- \textit{tkv}}{Normalized \, TE)}$)')
      
      selected_gene_data_norm = 
        selected_gene_data %>% 
        dplyr::group_by(FBGN, Symbol) %>% 
        dplyr::mutate(Norm_expression = log2((Mean_expression+1)/(Mean_expression[Genotype=="UAS-tkv"]+1)))
      
      selected_gene_data_norm$Genotype = factor(x = selected_gene_data$Genotype, 
                                                levels = genotype_levels)
      
      comparisons = list(c("UAS-tkv", "bam RNAi"), 
                         c("UAS-tkv", "bam RNAi; HS-bam"), 
                         c("UAS-tkv", "Young WT"))
      
      stats = selected_gene_data_norm %>% 
        ungroup() %>% 
        rstatix::t_test(formula = Norm_expression~Genotype, comparisons = comparisons, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", step.increase = 0.5) 
      
    }else if(normalization == "unNorm"){
      yaxis_label = expression("log"[2]*"(TE)")
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(Norm_expression = log2(Mean_expression))
      
      stats = selected_gene_data_norm %>% 
        ungroup() %>% 
        wilcox_test(formula = Norm_expression~Genotype, paired = TRUE, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", dodge = 0.8, step.increase = 0.7)
    }
  }else if(data_set_to_plot=="Single_cell_seq_germline"){
    if(normalization == "each_gene"){
      yaxis_label = expression("log normalized expression to GSC/CB/2CC")
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::group_by(FBGN, Symbol) %>% 
        dplyr::mutate(Norm_expression = log2((Mean_expression+1)/(Mean_expression[Genotype=="GSC/CB/2CC"]+1)))
      
      selected_gene_data_norm$Genotype = factor(x = selected_gene_data$Genotype, 
                                                levels = genotype_levels)
      
      comparisons = list(c("GSC/CB/2CC", "4CC"), 
                         c("GSC/CB/2CC", "8CC"), 
                         c("GSC/CB/2CC", "16CC\n2a 1"),
                         c("GSC/CB/2CC", "16CC\n2a 2"),
                         c("GSC/CB/2CC", "16CC\n2ab"),
                         c("GSC/CB/2CC", "16CC\n2b"),
                         c("GSC/CB/2CC", "16CC\n3"),
                         c("GSC/CB/2CC", "St2"))
      
      stats = selected_gene_data_norm %>% 
        ungroup() %>% 
        rstatix::t_test(formula = Norm_expression~Genotype, comparisons = comparisons, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", step.increase = 0.5) 
      
    }else if(normalization == "unNorm"){
      yaxis_label = expression("log normalized expression")
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(Norm_expression = Mean_expression)
      
      stats = selected_gene_data_norm %>% 
        ungroup() %>% 
        wilcox_test(formula = Norm_expression~Genotype, paired = TRUE, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", dodge = 0.8, step.increase = 0.7)
    }
  }else if (data_set_to_plot == "Single_cell_seq_soma"){
    
    if(normalization == "each_gene"){
      yaxis_label = expression("log normalized expression to TF/CC")
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::group_by(FBGN, Symbol) %>% 
        dplyr::mutate(Norm_expression = log2((Mean_expression+1)/(Mean_expression[Genotype=="TF/CC"]+1)))
      
      selected_gene_data_norm$Genotype = factor(x = selected_gene_data$Genotype, 
                                                levels = genotype_levels)
      
      comparisons = list(c("TF/CC", "aEc"), 
                         c("TF/CC", "cEc"), 
                         c("TF/CC", "pEc"),
                         c("TF/CC", "FSC/pre-FC"),
                         c("TF/CC", "pre-stalk"),
                         c("TF/CC", "stalk"),
                         c("TF/CC", "polar"))
      
      stats = selected_gene_data_norm %>% 
        ungroup() %>% 
        rstatix::t_test(formula = Norm_expression~Genotype, comparisons = comparisons, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", step.increase = 0.5) 
      
    }else if(normalization == "unNorm"){
      yaxis_label = expression("Relative log fold change")
      selected_gene_data_norm = selected_gene_data %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(Norm_expression = Mean_expression)
      
      stats = selected_gene_data_norm %>% 
        ungroup() %>% 
        wilcox_test(formula = Norm_expression~Genotype, paired = TRUE, p.adjust.method = "holm") %>%
        add_xy_position(x = "Genotype", dodge = 0.8, step.increase = 0.7)
    }
  }
  # order factors
  selected_gene_data_norm$Genotype = factor(x = selected_gene_data$Genotype, 
                                            levels = genotype_levels)
  # Data for download
  selected_gene_data_norm_global <<- selected_gene_data_norm
  
  # violin plot with points for each gene
  gene_violin_plot = 
    ggplot(data = selected_gene_data_norm, mapping = aes(x = Genotype, y = Norm_expression))+
    geom_violin()+
    xlab("")+
    ylab(yaxis_label)+
    scale_x_discrete(labels = formatted_labels)+
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
    stat_summary(mapping = aes(group = Genotype), 
                 fun = median, fun.min = median, fun.max = median,
                 geom = "crossbar", width = 0.4)+
    geom_point(position = position_jitter(seed = 1, width = 0.2), 
               color="grey60", shape = 1, alpha = 0.7)+
    theme_white()+
    theme(axis.text.x = element_text(size = text_scale),
          axis.text.y = element_text(size = text_scale), 
          axis.title.x = element_text(size = text_scale),
          axis.title.y = element_text(size = text_scale))
  
  if(normalization == "each_gene"){
    stats_flat = stats
    stats_flat$y.position = min(stats$y.position)+0.5+(text_scale/48)+pval_yadj-2
    
    gene_violin_plot = gene_violin_plot+
      add_pvalue(stats_flat, label.size = text_scale/3, label = "p.adj", tip.length = 0.0, remove.bracket = TRUE)+
      annotate(geom = "segment", x = 1, xend = mean(stats$xmax)-text_scale/48, 
               y = min(stats$y.position)+pval_yadj, yend = min(stats$y.position)+pval_yadj)+
      annotate(geom = "segment", x = min(stats$xmax), xend = max(stats$xmax), 
               y = min(stats$y.position), yend = min(stats$y.position))+
      annotate(geom = "segment", x = 1, xend = 1, 
               y = min(stats$y.position), yend = min(stats$y.position)+pval_yadj)+
      annotate(geom = "segment", x = mean(stats$xmax)-text_scale/48, xend = mean(stats$xmax)-text_scale/48, 
               y = min(stats$y.position), yend = min(stats$y.position)+pval_yadj)
  }else if(normalization == "unNorm"){
    gene_violin_plot = gene_violin_plot+
      add_pvalue(stats, label.size = text_scale/3, label = "p.adj", tip.length = 0.0)
  }
  .GlobalEnv$violin_plot_dataset_plotted = data_set_to_plot
  return(gene_violin_plot)
}
