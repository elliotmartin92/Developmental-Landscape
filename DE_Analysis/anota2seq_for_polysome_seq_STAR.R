library(anota2seq)

temp = list.files(path = "fastqs/STAR_processed_files/", pattern = "*ReadsPerGene.out.tab$", recursive = TRUE)
data.table(temp)
pick = temp[c(15:18, 9:14, 1:8, 19:22)] #pick or reorder featurecounts
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
# genotype = factor(genotype, levels = unique(genotype))


rep = sapply(strsplit(picked_names, "_"),`[`,3)


# 
# unique_genotype = unique(genotype)
# pairwise_cols = c()
# col_plus1 = c()
# N = length(unique_genotype)
# for( i in 1:(N-1) ){
#   pairwise_cols = c(pairwise_cols, unique_genotype[((1+i):(N))])
#   col_plus1 = c(col_plus1, (1+i):(N))
# }
# 
# pairwise_matrix = matrix(0L, ncol = length(pairwise_cols), nrow = length(unique_genotype))
# colnames(pairwise_matrix) = pairwise_cols
# row.names(pairwise_matrix) = unique_genotype
# 
# N2 = length(pairwise_matrix)
# row_fill = 1:N2
# col_min1 = c()
# for (i in 1:(N2-3)) {
#   col_min1 = c(col_min1, rep(i, (4-i)))
# }
# 
# for (k in 1:N2) {
#   pairwise_matrix[col_min1[k], row_fill[k]] = -1
#   pairwise_matrix[col_plus1[k], row_fill[k]] = +1
# }
# pairwise_matrix

# Get the levels of the phenoVec, these will be ordered as in anota2seq
phenoLev <- levels(as.factor(genotype))
# Construct the matrix with appropriate nrow and ncol
myContrast <- matrix(nrow =length(phenoLev),ncol=length(phenoLev)-1)
# Set the phenoLev as rownames for your contrast matrix
rownames(myContrast) <- phenoLev
# Now indicate the contrasts you want to analyse as explained above

myContrast[,1] = c(-1, 0, 1, 0)
myContrast[,2] = c(0, -1, 1, 0)
myContrast[,3] = c(0, 0, 1, -1)
myContrast

ads <- anota2seqDataSetFromMatrix(
  dataP = polysome,
  dataT = input,
  phenoVec = genotype,
  dataType = "RNAseq",
  normalize = TRUE, 
  filterZeroGenes = FALSE)

ads <- anota2seqRun(ads, onlyGroup = TRUE, contrasts = myContrast)
anota2seqPlotFC(ads, selContrast = 1, plotToFile = TRUE)

c3 = 
  anota2seqGetOutput(
    ads, analysis = "total mRNA", output = "selected", getRVM = TRUE,
    selContrast = 1)[, c("apvEff", "apvRvmPAdj", "singleRegMode")]

c3 %>% filter(apvEff > 2 | apvEff < -2) %>% filter(singleRegMode != "none") %>% rownames() %>% writeClipboard()

bam = myContrast
bam[,1] = c(-1, 1, 0, 0)
bam[,2] = c(0, 1, -1, 0)
bam[,3] = c(0, 1, 0, -1)
bam

ads_Bam <- anota2seqDataSetFromMatrix(
  dataP = polysome,
  dataT = input,
  phenoVec = genotype,
  dataType = "RNAseq",
  normalize = TRUE, 
  filterZeroGenes = FALSE)

ads_Bam <- anota2seqRun(ads_Bam, onlyGroup = TRUE, contrasts = bam)

c3 = 
  anota2seqGetOutput(
    ads, analysis = "total mRNA", output = "selected", getRVM = TRUE,
    selContrast = 3)[, c("apvEff", "apvRvmPAdj", "singleRegMode")]

c3 %>% filter(apvEff > 1 | apvEff < -1) %>% filter(singleRegMode == "translation") %>% rownames() %>% writeClipboard()
