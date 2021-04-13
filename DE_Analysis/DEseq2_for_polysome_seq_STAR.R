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
res <- results(dds)
resTable <- data.table(rownames(res), as.data.table(res))
pvalue_cutoff = 0.1
log2FC_cutoff = 1
up = resTable %>% filter(pvalue < pvalue_cutoff) %>% filter(log2FoldChange > log2FC_cutoff)
down = resTable %>% filter(pvalue < pvalue_cutoff) %>% filter(log2FoldChange < -log2FC_cutoff)
changing_genes = c(up$V1, down$V1)
write_rds(changing_genes, file = "ShinyExpresionMap/Preprocessed_data/developmentally_regulated_gene_list_polysome.RDS")
