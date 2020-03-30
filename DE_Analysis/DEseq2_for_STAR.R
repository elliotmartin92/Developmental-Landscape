library(DESeq2)
library(data.table)
library(dplyr)
library(plyr)
library(org.Dm.eg.db)
library(annotate)
source("Y:/Data/ElliotMartin/rscripts/Finished Scripts/ggplotWhiteTheme.R")

temp = list.files(path = "../fastqs/STAR_processed_files/", pattern = "*ReadsPerGene.out.tab", recursive = TRUE)
data.table(temp)
pick = temp[] #pick or reorder featurecounts
data.table(pick)
temp_names = unlist(strsplit(pick, 'ReadsPerGene.out.tab'))
picked_names = unlist(strsplit(temp_names, '/'))[seq(2, length(temp_names)*2, 2)]
groups = strsplit(picked_names, '*_[0-9]')
myfiles = lapply(paste0("../fastqs/STAR_processed_files/", pick),
                 read.delim,
                 stringsAsFactors = F,
                 header = F)
allseq = data.frame(myfiles)
rownames(allseq) = allseq[[1]]
allseq = allseq[c(seq(from = 4, to = length(allseq), by = 4))]
colnames(allseq) = picked_names
allseq = allseq[-(1:4),]

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
                             design =  ~ all)

dds <- DESeq(dds)

rld <- rlog(dds, blind = FALSE)
head(assay(rld), 3)
plotPCA(rld, intgroup = c("all"))

fbgn_to_symbol =  function(fbid){
  AnnotationDbi::select(org.Dm.eg.db, fbid, 
                        columns=c("SYMBOL"), 
                        keytype="FLYBASE") %>% data.table()
}

comparisons = 

res <- results(dds, contrast = c("all", "BamHSbam_input", "youngWT_input")) #change genotypes to desired
resTable <- data.table(rownames(res), as.data.table(res))
up = resTable %>% filter(padj<.05) %>% filter(log2FoldChange > 1.5)
down = resTable %>% filter(padj<.05) %>% filter(log2FoldChange < -1.5)

library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "AllGenes")
addWorksheet(wb, "UpregulatedGenes")
addWorksheet(wb, "DownregulatedGenes")
writeData(wb, 1, resTable)
writeData(wb, 2, up)
writeData(wb, 3, down)
saveWorkbook(wb, file = "CGvsBam.xlsx", overwrite = TRUE)


toplot = resTable
toplot$color = "NC"
toplot$color[toplot$FLYBASE %in% down$FLYBASE] = "down"
toplot$color[toplot$FLYBASE %in% up$FLYBASE] = "up"
toplot$color = factor(toplot$color, levels = unique(toplot$color))


library(ggrastr)
ggplot()+
  geom_point_rast(data = toplot, aes(log2FoldChange, -log10(padj), color=color), alpha = 0.5)+
  scale_color_manual(values = c("grey","dodgerblue4", "darkred"))+
  theme_white()+
  theme(legend.position = "none")
# ggsave(filename = "CGvsBamVolcano.pdf", width = 3, height = 3)
