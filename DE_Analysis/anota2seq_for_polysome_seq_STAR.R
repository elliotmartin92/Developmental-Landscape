library(anota2seq)

temp = list.files(path = "fastqs/STAR_processed_files/", pattern = "*ReadsPerGene.out.tab$", recursive = TRUE)
data.table(temp)
pick = temp[] #pick or reorder featurecounts
data.table(pick)
temp_names = unlist(strsplit(pick, 'ReadsPerGene.out.tab'))
picked_names = unlist(strsplit(temp_names, '/'))[seq(2, length(temp_names)*2, 2)]

myfiles = lapply(paste0("fastqs/STAR_processed_files/", pick),
                 read.delim,
                 stringsAsFactors = F,
                 header = F)
allseq = data.frame(myfiles)
rownames(allseq) = allseq[[1]]
allseq = allseq[c(seq(from = 4, to = length(allseq), by = 4))]
colnames(allseq) = picked_names
allseq = allseq[-(1:4),] #remove header/summary rows

type = sapply(strsplit(picked_names, "_"),`[`,2)
input = allseq[,type=="input"]
polysome = allseq[,type=="polysome"]

genotype = sapply(strsplit(names(input), "_"),`[`,1)


rep = sapply(strsplit(picked_names, "_"),`[`,3)



unique_genotype = unique(genotype)
pairwise_cols = c()
col_plus1 = c()
N = length(unique_genotype)
for( i in 1:(N-1) ){
  pairwise_cols = c(pairwise_cols, unique_genotype[((1+i):(N))])
  col_plus1 = c(col_plus1, (1+i):(N))
}

pairwise_matrix = matrix(0L, ncol = length(pairwise_cols), nrow = length(unique_genotype))
colnames(pairwise_matrix) = pairwise_cols
row.names(pairwise_matrix) = unique_genotype

N2 = length(pairwise_matrix)
row_fill = 1:N2
col_min1 = c()
for (i in 1:(N2-3)) {
  col_min1 = c(col_min1, rep(i, (4-i)))
}

for (k in 1:N2) {
  pairwise_matrix[col_min1[k], row_fill[k]] = -1
  pairwise_matrix[col_plus1[k], row_fill[k]] = +1
}
pairwise_matrix

ads <- anota2seqDataSetFromMatrix(
  dataP = polysome,
  dataT = input,
  phenoVec = genotype,
  dataType = "RNAseq",
  normalize = TRUE, 
  filterZeroGenes = FALSE)
ads <- anota2seqRun(ads, onlyGroup = TRUE, contrasts = pairwise_matrix)
anota2seqPlotFC(ads, selContrast = 1, plotToFile = TRUE)

c3 = 
  anota2seqGetOutput(
    ads, analysis = "translation", output = "selected", getRVM = TRUE,
    selContrast = 1)[, c("apvEff", "apvRvmPAdj", "singleRegMode")]




