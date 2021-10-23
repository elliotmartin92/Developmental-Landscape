library(tidyverse)

original = list.files(path = "fastqs/STAR_processed_files/", pattern = "*ReadsPerGene.out.tab$", recursive = TRUE, full.names = TRUE)
flybase = list.files(path = "fastqs/STAR_processed_files_FLYBASE", pattern = "*ReadsPerGene.out.tab$", recursive = TRUE, full.names = TRUE)

original_table = read.delim(original[1], stringsAsFactors = F, header = F)
flybase_table = read.delim(flybase[1], stringsAsFactors = F, header = F)

original_table_gene = original_table[-(1:4),]
flybase_table_gene = flybase_table[-(1:4),]

table_gene_merge = full_join(original_table_gene, flybase_table_gene, by="V1")

tail(table_gene_merge[is.na(table_gene_merge$V4.x == table_gene_merge$V4.y),])
