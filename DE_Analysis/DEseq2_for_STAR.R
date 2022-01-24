library(DESeq2)
library(openxlsx)
library(data.table)
library(tidyverse)
library(org.Dm.eg.db)
library(annotate)
source("Y:/Data/ElliotMartin/rscripts/Finished Scripts/ggplotWhiteTheme.R")

temp = list.files(path = "fastqs/STAR_processed_files/", pattern = "*ReadsPerGene.out.tab$", recursive = TRUE) #find files with correct ending
data.table(temp)
pick = temp[] #pick or reorder featurecounts
data.table(pick)
temp_names = unlist(strsplit(pick, 'ReadsPerGene.out.tab'))
picked_names = unlist(strsplit(temp_names, '/'))[seq(2, length(temp_names)*2, 2)] #extract cleaned names from file names

myfiles = lapply(paste0("fastqs/STAR_processed_files/", pick), #read count files
                 read.delim,
                 stringsAsFactors = F,
                 header = F)
allseq = data.frame(myfiles) #count files to df
rownames(allseq) = allseq[[1]]  #change rownames to FBids
allseq = allseq[c(seq(from = 4, to = length(allseq), by = 4))]  #extract columns containing counts 
colnames(allseq) = picked_names
allseq = allseq[-(1:4),] #remove header/summary rows
allseq = allseq["input"==sapply(strsplit(picked_names, "_"),`[`,2)]

genotype = sapply(strsplit(colnames(allseq), "_"),`[`,1) #extract grouping variables from names
type = sapply(strsplit(colnames(allseq), "_"),`[`,2)
rep = sapply(strsplit(colnames(allseq), "_"),`[`,3)

sampleTable = data.frame(genotype=factor(genotype), type = factor(type)) #assemble sample table for deseq2

#assemble design table for deseq2
design = data.frame(row.names=colnames(allseq), genotype = sampleTable$genotype, type = sampleTable$type, 
                    all=(paste(sampleTable$genotype, sampleTable$type, sep="_")))
design
#deseq2 model design
dds = DESeqDataSetFromMatrix(countData = allseq,
                             colData = design,
                             design =  ~ all)
# run deseq2
dds <- DESeq(dds)
# perform pca
rld <- rlog(dds, blind = FALSE)
head(assay(rld), 3)
plotPCA(rld, intgroup = c("all"))

# function to convert fbgn to symbols
fbgn_to_symbol =  function(fbid){
  AnnotationDbi::select(org.Dm.eg.db, fbid, 
                        columns=c("SYMBOL"), 
                        keytype="FLYBASE") %>% data.table()
}

# function to extract pairwise comparisions from deseq2 dds object
pairwise_dds = function(GenotypeA, GenotypeB, padj_cutoff=0.05, log2FC_cutoff=2){
  if(GenotypeA==GenotypeB){return(NA)}
  res <- results(dds, contrast = c("all", GenotypeA, GenotypeB))
  resTable <- data.table(rownames(res), as.data.table(res))
  resTable = dplyr::rename(resTable, FBGN = "V1")
  fb_symbols = fbgn_to_symbol(resTable$FBGN)[[2]]
  resTable = add_column(resTable, symbol=fb_symbols, .after = "FBGN")
  up = resTable %>% filter(padj < padj_cutoff) %>% filter(log2FoldChange > log2FC_cutoff)
  down = resTable %>% filter(padj < padj_cutoff) %>% filter(log2FoldChange < -log2FC_cutoff)
  
  GenotypeA_name = strsplit(GenotypeA, "_")[[1]][1]
  GenotypeB_name = strsplit(GenotypeB, "_")[[1]][1]
  
  addWorksheet(wb, sheetName = paste("down in", GenotypeA_name, "vs", GenotypeB_name))
  writeData(wb, paste("down in", GenotypeA_name, "vs", GenotypeB_name), down)
  
  addWorksheet(wb, sheetName = paste("up in", GenotypeA_name, "vs", GenotypeB_name))
  writeData(wb, paste("up in", GenotypeA_name, "vs", GenotypeB_name), up)
  
  addWorksheet(wb, sheetName = paste("all", GenotypeA_name, "vs", GenotypeB_name))
  writeData(wb, paste("all", GenotypeA_name, "vs", GenotypeB_name), resTable)
  
  changing_genes = c(up$FBGN, down$FBGN)
  return(changing_genes)
}
# extract samples to be compared
comparison_samples = unique(design$all[grep(pattern = "input", x = design$all)])
comparison_samples = comparison_samples[c(3,2,1,4)]
number_comparisons = (length(comparison_samples)*((length(comparison_samples)-1)))/2 #number of pairwise comparisons
# initialize excel workbook prior to pairwise_dds call
wb = createWorkbook()
# pairwise_dds call
all_pairwise_comparisions = sapply(comparison_samples, function(GenotypeA) sapply(comparison_samples, function(GenotypeB) pairwise_dds(GenotypeA, GenotypeB)))
saveWorkbook(wb, file = "DE_Analysis/input_DE_pairwise_comparisions.xlsx", overwrite = TRUE) #save workbook
head(all_pairwise_comparisions)
# flatten pairwise comparisons and remove redundant entries
all_pairwise_comparisions_uniqueflat = unique(unlist(c(all_pairwise_comparisions)))
head(all_pairwise_comparisions_uniqueflat)
# write all DE genes to an RDS
write_rds(all_pairwise_comparisions_uniqueflat, 
          file = "ShinyExpresionMap/Preprocessed_data/Input_4fold_p0.05_developmentally_regulated_gene_list.RDS")
