library(Seurat)
library(tidyverse)
library(openxlsx)
load("TPMs/GC_clusters.RData")

all_de = FindAllMarkers(GC_clusters, logfc.threshold = 0.75)
all_de_genes = all_de %>% pull(gene)

write_rds(all_de_genes, "ShinyExpresionMap/Preprocessed_data/Single_cell_seq_regulated_gene_list.RDS")
write.xlsx(all_de, "DE_Analysis/single_cell/single_cell_DE.xlsx")
GC_clusters

all_genes_vs_cluster1 = FindAllMarkers(GC_clusters, logfc.threshold = 0, min.pct = 0)