library(data.table)
library(stringr)
library(tidyverse)

temp = list.files(path = "fastqs/STAR_processed_files/", pattern = "*ReadsPerGene.out.tab", recursive = TRUE)
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
allseq = allseq[-(1:4),]
head(allseq)

####TPM####
tpm = function(counts, lengths) {
  rate = counts / lengths
  rate / sum(rate) * 1e6
}

featLens = read.table(file = "TPMs/Feature_lengths_dm6ensgene.tabular",  header = TRUE, stringsAsFactors = FALSE)


is_input = !is.na(str_extract(picked_names, "input"))

tpms = data.frame(apply(X = allseq, 2, tpm, lengths=featLens[[2]]))
rownames(tpms) = rownames(allseq)
tpms_inputs = tpms[is_input]
tpms_inputs_tib = as_tibble(data.frame(FBGN = rownames(tpms_inputs), tpms_inputs))
head(tpms_inputs_tib)
write_rds(tpms_inputs_tib, "ShinyExpresionMap/raw_tpms_input.rds")

tpms_inputs_long = tpms_inputs_tib %>% pivot_longer(cols = -FBGN, names_to = "Genotype", values_to = "TPM")
groups = unlist(strsplit(tpms_inputs_long$Genotype, '*_[0-9]')) 
tpms_inputs_long$Group = groups

std <- function(x) sd(x)/sqrt(length(x))
tpms_long_mean = tpms_inputs_long %>% 
  group_by(Group, FBGN) %>% 
  summarise(MeanTPM = mean(TPM), se = std(TPM))

tpms_long_mean$MeanTPMpmError = paste0(round(signif(tpms_long_mean$MeanTPM, digits = 3), digits = 1), "±", 
                                       round(signif(tpms_long_mean$se, digits = 3), digits = 1))
tpms_long_mean$Group = factor(tpms_long_mean$Group, 
                              levels = c("TKV_input", "BamRNAi_input", "BamHSbam_input", "youngWT_input", "pelo_cyo_input"))

tpms_wide_mean = tpms_long_mean %>% 
  arrange(Group) %>% 
  dplyr::select(-se) %>% 
  pivot_wider(names_from = Group, values_from = c(MeanTPM, MeanTPMpmError))

saveRDS(tpms_wide_mean, file = "TPMs/Mean_TPMs_and_text.RDS")
