library(Seurat)
library(tidyverse)
load("TPMs/GC_clusters.RData")

all_de = FindAllMarkers(GC_clusters, logfc.threshold = 0.75)
all_de_genes = all_de %>% pull(gene)

write_rds(all_de_genes, "ShinyExpresionMap/Preprocessed_data/Single_cell_seq_regulated_gene_list.RDS")
