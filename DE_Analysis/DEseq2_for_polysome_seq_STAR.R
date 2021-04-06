library(DESeq2)
library(data.table)
library(tidyverse)
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

dds <- DESeq(dds, test="LRT", reduced = ~ type + genotype)
resultsNames(dds)


rld <- rlog(dds, blind = FALSE)
head(assay(rld), 3)
plotPCA(rld, intgroup = c("all"))

fbgn_to_symbol =  function(fbid){
  AnnotationDbi::select(org.Dm.eg.db, fbid, 
                        columns=c("SYMBOL"), 
                        keytype="FLYBASE") %>% data.table()
}

pairwise_dds = function(GenotypeA, GenotypeB, padj_cutoff=0.05, log2FC_cutoff=2){
  if(GenotypeA==GenotypeB){return(NA)}
  res <- results(dds, contrast = c("all", GenotypeA, GenotypeB))
  resTable <- data.table(rownames(res), as.data.table(res))
  up = resTable %>% filter(padj < padj_cutoff) %>% filter(log2FoldChange > log2FC_cutoff)
  down = resTable %>% filter(padj < padj_cutoff) %>% filter(log2FoldChange < -log2FC_cutoff)
  changing_genes = c(up$V1, down$V1)
  return(changing_genes)
}

comparison_samples = unique(design$all[grep(pattern = "input", x = design$all)])
number_comparisons = (length(comparison_samples)*((length(comparison_samples)-1)))/2 #number of pairwise comparisons

all_pairwise_comparisions = sapply(comparison_samples, function(GenotypeA) sapply(comparison_samples, function(GenotypeB) pairwise_dds(GenotypeA, GenotypeB)))
head(all_pairwise_comparisions)
all_pairwise_comparisions_uniqueflat = unique(unlist(c(all_pairwise_comparisions)))
head(all_pairwise_comparisions_uniqueflat)
write_rds(all_pairwise_comparisions_uniqueflat, file = "ShinyExpresionMap/developmentally_regulated_gene_list_polysome.RDS")

# res <- results(dds, contrast = c("all", "BamHSbam_input", "youngWT_input")) #change genotypes to desired
# resTable <- data.table(rownames(res), as.data.table(res))
# up = resTable %>% filter(padj<.05) %>% filter(log2FoldChange > 1.5)
# down = resTable %>% filter(padj<.05) %>% filter(log2FoldChange < -1.5)
