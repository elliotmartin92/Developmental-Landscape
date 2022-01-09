library(DESeq2)
library(data.table)
library(tidyverse)
library(openxlsx)
library(org.Dm.eg.db)
library(annotate)
source("Y:/Data/ElliotMartin/rscripts/Finished Scripts/ggplotWhiteTheme.R")

temp = list.files(path = "fastqs/STAR_processed_files/", pattern = "*ReadsPerGene.out.tab$", recursive = TRUE)
data.table(temp)
pick = temp[] #pick or reorder featurecounts
data.table(pick)
temp_names = unlist(strsplit(pick, 'ReadsPerGene.out.tab'))
picked_names = unlist(strsplit(temp_names, '/'))[seq(2, length(temp_names)*2, 2)]

groups = strsplit(picked_names, '*_[0-9]')
myfiles = lapply(paste0("fastqs/STAR_processed_files/", pick),
                 read.delim,
                 stringsAsFactors = F,
                 header = F)
allseq = data.frame(myfiles)
rownames(allseq) = allseq[[1]]
allseq = allseq[c(seq(from = 4, to = length(allseq), by = 4))]
colnames(allseq) = picked_names
allseq = allseq[-(1:4),] #remove header/summary rows

# allseq = allseq[!apply(allseq, MARGIN = 1, FUN = function(y) any(y==0)),]

genotype = sapply(strsplit(picked_names, "_"),`[`,1)

type = sapply(strsplit(picked_names, "_"),`[`,2)
rep = sapply(strsplit(picked_names, "_"),`[`,3)

sampleTable = data.frame(genotype=factor(genotype), type = factor(type))


design = data.frame(row.names=picked_names, genotype = sampleTable$genotype, type = sampleTable$type,
                    all=(paste(sampleTable$genotype, sampleTable$type, sep="_")))
design

dds = DESeqDataSetFromMatrix(countData = allseq,
                             colData = design,
                             design =  ~ type + genotype + genotype:type)
plotPCA(dds)

dds = DESeq(dds, test="LRT", reduced = ~ type + genotype)
resultsNames(dds)

# function to convert fbgn to symbols
fbgn_to_symbol =  function(fbid){
  AnnotationDbi::select(org.Dm.eg.db, fbid, 
                        columns=c("SYMBOL"), 
                        keytype="FLYBASE") %>% data.table()
}

pairwise_dds = function(GenotypeA, GenotypeB, pval_cutoff=0.05, log2FC_cutoff=1){
  if(GenotypeA==GenotypeB){return(NA)}
  res <- results(dds, contrast = c("genotype", GenotypeA, GenotypeB))
  resTable <- as_tibble(data.table(rownames(res), as.data.table(res)))
  resTable = dplyr::rename(resTable, FBGN = "V1")
  fb_symbols = fbgn_to_symbol(resTable$FBGN)[[2]]
  resTable = add_column(resTable, symbol=fb_symbols, .after = "FBGN")
  up = resTable %>% filter(pvalue < pval_cutoff) %>% filter(log2FoldChange > log2FC_cutoff)
  down = resTable %>% filter(pvalue < pval_cutoff) %>% filter(log2FoldChange < -log2FC_cutoff)

  addWorksheet(wb, sheetName = paste("down in", GenotypeA, "vs", GenotypeB))
  writeData(wb, paste("down in", GenotypeA, "vs", GenotypeB), down)

  addWorksheet(wb, sheetName = paste("up in", GenotypeA, "vs", GenotypeB))
  writeData(wb, paste("up in", GenotypeA, "vs", GenotypeB), up)

  addWorksheet(wb, sheetName = paste("all", GenotypeA, "vs", GenotypeB))
  writeData(wb, paste("all", GenotypeA, "vs", GenotypeB), resTable)

  changing_genes = c(up$FBGN, down$FBGN)
  return(changing_genes)
}

# extract samples to be compared
comparison_samples = as.character(unique(design$genotype))
comparison_samples = comparison_samples[c(3,2,1,4)]
number_comparisons = (length(comparison_samples)*((length(comparison_samples)-1)))/2 #number of pairwise comparisons
# initialize excel workbook prior to pairwise_dds call
wb = createWorkbook()
# pairwise_dds call
all_pairwise_comparisions = sapply(comparison_samples, function(GenotypeA) sapply(comparison_samples, function(GenotypeB) pairwise_dds(GenotypeA, GenotypeB)))
saveWorkbook(wb, file = "DE_Analysis/polysome_DE_pairwise_comparisions.xlsx", overwrite = TRUE) #save workbook
head(all_pairwise_comparisions)
# flatten pairwise comparisons and remove redundant entries
all_pairwise_comparisions_uniqueflat = unique(unlist(c(all_pairwise_comparisions)))
head(all_pairwise_comparisions_uniqueflat)
# write all DE genes to an RDS
# write_rds(all_pairwise_comparisions_uniqueflat, file = "ShinyExpresionMap/polysome_developmentally_regulated_gene_list.RDS")

trbl = function(GenotypeA, GenotypeB){
  if(GenotypeA==GenotypeB){return(NA)}
  print(c(GenotypeA, GenotypeB))
}
