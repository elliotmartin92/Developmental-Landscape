library(data.table)
library(stringr)

temp = list.files(path = "fastqs/bkp.STAR_processed_files/", pattern = "*ReadsPerGene.out.tab", recursive = TRUE)
data.table(temp)
pick = temp[-10] #pick or reorder featurecounts
data.table(pick)
temp_names = unlist(strsplit(pick, 'ReadsPerGene.out.tab'))
picked_names = unlist(strsplit(temp_names, '/'))[seq(2, length(temp_names)*2, 2)]
myfiles = lapply(paste0("fastqs/bkp.STAR_processed_files/", pick),
                 read.delim,
                 stringsAsFactors = F,
                 header = F)
allseq = data.frame(myfiles)
rownames(allseq) = allseq[[1]]
allseq = allseq[c(seq(from = 4, to = length(allseq), by = 4))]
colnames(allseq) = picked_names
allseq = allseq[-(1:4),]
head(allseq)

groups = unlist(strsplit(picked_names, '*_[0-9]'))
type = is.na(str_extract(picked_names, "input"))
