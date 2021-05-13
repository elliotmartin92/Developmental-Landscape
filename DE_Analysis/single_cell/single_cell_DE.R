library(Seurat)
library(tidyverse)
library(openxlsx)
load("Normalized_expression/GC_clusters.RData")
load("Normalized_expression/germarium_clusters.RData")

####Prepare data for heatmap - GC cluster####
DefaultAssay(object = GC_clusters) <- "RNA" #set slot to RNA
all_de_GC = FindAllMarkers(GC_clusters, logfc.threshold = 0.75) #DE with 0.75 logFC cutoff
all_de_genes_GC = all_de_GC %>% pull(gene) #extract genes that are DE as list

# generate all relative log2FC values for every gene in every cluster
all_fold_changes_GC = FindAllMarkers(GC_clusters, logfc.threshold = 0, min.pct = -Inf, return.thresh = 1)

# subset all_fold_changes_GC to only genes that are DE
heat_data_GC = all_fold_changes_GC %>% 
  ungroup() %>% 
  select(gene, avg_log2FC, cluster) %>%
  pivot_wider(id_cols = gene, names_from = cluster, values_from = avg_log2FC) %>% 
  filter(gene %in% all_de_genes_GC) %>% 
  column_to_rownames(var = "gene") %>% 
  as.data.frame()

# Add FBGNs to all FC
conversion_table = read_tsv("Normalized_expression/Symbol_to_FBID_table_sc_seq.tsv", col_names = c("FBGN", "Symbol"))
all_fold_changes_conversion_GC = 
  left_join(x = all_fold_changes_GC, y = conversion_table, by = c("gene"="Symbol")) %>% 
  rename(Symbol=gene) %>% 
  relocate(FBGN:Symbol, .before = p_val) %>% 
  select(c(1, 2, 4, 8)) %>% 
  pivot_wider(id_cols = c(Symbol, FBGN), names_from = "cluster", values_from = "avg_log2FC")

# write out generated data to deliver to heatmap/violin generation preprocessing script
write_rds(all_fold_changes_conversion_GC, "ShinyExpresionMap/Preprocessed_data/single_cell_seq_fold_changes_GC.RDS")
write_rds(all_de_genes_GC, "ShinyExpresionMap/Preprocessed_data/single_cell_seq_regulated_gene_list_GC.RDS")
write_rds(heat_data_GC, "ShinyExpresionMap/Preprocessed_data/single_cell_seq_regulated_gene_fold_change_list_GC.RDS")
write.xlsx(all_de_GC, "DE_Analysis/single_cell/GC_single_cell_DE.xlsx")

####Prepare data for heatmap - germarium soma cluster####
# procedure is identical as above for GC cluster
DefaultAssay(object = germarium_clusters) <- "RNA" 
all_de_soma = FindAllMarkers(germarium_clusters, logfc.threshold = 0.75)
all_de_genes_soma = all_de_soma %>% pull(gene)

all_fold_changes_soma = FindAllMarkers(germarium_clusters, logfc.threshold = 0, min.pct = -Inf, return.thresh = 1)

heat_data_soma = all_fold_changes_soma %>% 
  ungroup() %>% 
  select(gene, avg_log2FC, cluster) %>%
  pivot_wider(id_cols = gene, names_from = cluster, values_from = avg_log2FC) %>% 
  filter(gene %in% all_de_genes_soma) %>% 
  column_to_rownames(var = "gene") %>% 
  as.data.frame()

# Add FBGNs to all FC
conversion_table = read_tsv("Normalized_expression/Symbol_to_FBID_table_sc_seq.tsv", col_names = c("FBGN", "Symbol"))
all_fold_changes_conversion_soma = 
  left_join(x = all_fold_changes_soma, y = conversion_table, by = c("gene"="Symbol")) %>% 
  rename(Symbol=gene) %>% 
  relocate(FBGN:Symbol, .before = p_val) %>% 
  select(c(1, 2, 4, 8)) %>% 
  pivot_wider(id_cols = c(Symbol, FBGN), names_from = "cluster", values_from = "avg_log2FC")

write_rds(all_fold_changes_conversion_soma, "ShinyExpresionMap/Preprocessed_data/single_cell_seq_fold_changes_germarium_soma.RDS")
write_rds(all_de_genes_soma, "ShinyExpresionMap/Preprocessed_data/Single_cell_seq_regulated_gene_list_germarium_soma.RDS")
write_rds(heat_data_soma, "ShinyExpresionMap/Preprocessed_data/single_cell_seq_regulated_gene_fold_change_list_germarium_soma.RDS")
write.xlsx(all_de_soma, "DE_Analysis/single_cell/germarium_soma_single_cell_DE.xlsx")

