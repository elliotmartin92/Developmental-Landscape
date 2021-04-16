library(data.table)
library(stringr)
library(tidyverse)

# This script computes TPMs for all libraries and computes polysome/input ratio

temp = list.files(path = "fastqs/STAR_processed_files/", pattern = "*ReadsPerGene.out.tab$", recursive = TRUE)
data.table(temp)
pick = temp[] #pick or reorder featurecounts
data.table(pick)
temp_names = unlist(strsplit(pick, 'ReadsPerGene.out.tab'))
picked_names = unlist(strsplit(temp_names, '/'))[seq(2, length(temp_names)*2, 2)]
picked_names

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

####TPM####
tpm = function(counts, lengths) {
  rate = counts / lengths
  rate / sum(rate) * 1e6
}

featLens = read.table(file = "TPMs/Feature_lengths_dm6ensgene.tabular",  
                      header = TRUE, 
                      stringsAsFactors = FALSE)

tpms_all = data.table(apply(X = allseq, 2, tpm, lengths=featLens[[2]]))
rownames(tpms_all) = rownames(allseq)
tpms_all_tib = as_tibble(data.frame(FBGN = rownames(tpms_all), tpms_all))
head(tpms_all_tib)

tpms_all_long = tpms_all_tib %>% 
  pivot_longer(
    cols = -FBGN,
    names_to = c("Genotype", "Source", "Replicate"),
    names_sep  = "_",
    values_to = "TPM"
)

std <- function(x) sd(x)/sqrt(length(x))

tpms_long_mean = tpms_all_long %>% 
  group_by(FBGN, Genotype, Source) %>% 
  mutate(MeanTPM = mean(TPM),
         se = std(TPM))


tpms_long_mean$MeanTPMpmError = paste0(round(signif(tpms_long_mean$MeanTPM, digits = 3), digits = 1), "±", 
                                       round(signif(tpms_long_mean$se, digits = 3), digits = 1))

tpms_long_mean$Genotype = factor(tpms_long_mean$Genotype, 
                              levels = c("TKV", "BamRNAi", "BamHSbam", "youngWT"))

tpms_wide_mean = tpms_long_mean %>% 
  arrange(Genotype) %>% 
  dplyr::select(-se, -Replicate, -TPM) %>%
  distinct() %>% 
  pivot_wider(names_from = c(Genotype, Source), values_from = c(MeanTPM, MeanTPMpmError))

saveRDS(tpms_wide_mean, file = "TPMs/All_Mean_TPMs_and_text.RDS")

tpms_all_long

tpms_all_long$Genotype = factor(tpms_all_long$Genotype, 
                                levels = c("TKV", "BamRNAi", "BamHSbam", "youngWT"))

polysome_ratios = tpms_all_long %>% 
  group_by(FBGN, Genotype, Replicate) %>% 
  mutate(polysome_over_input = (TPM+1)/(TPM[Source=="input"]+1))

std <- function(x) sd(x)/sqrt(length(x))

polysome_ratios_mean = polysome_ratios %>% 
  group_by(Genotype, Source, FBGN) %>% 
  summarise(MeanTPM = mean(TPM), se = std(TPM),
            Mean_polysome_over_input = mean(polysome_over_input), se = std(polysome_over_input)) %>% 
  mutate()

mean_log2_polysome_input_ratio =
tpms_all_long %>% 
  group_by(FBGN, Genotype, Replicate) %>% 
  mutate(Log2_Plus1_Polysome_Input_Ratio = log2(TPM+1)-log2(TPM[Source=="input"])+1) %>% 
  filter(Source == "polysome") %>% 
  select(-c(Source, TPM)) %>%
  group_by(FBGN, Genotype) %>%
  summarise(Mean_Log2_Plus1_Polysome_Input_Ratio = mean(Log2_Plus1_Polysome_Input_Ratio)) %>% 
  arrange(Genotype) %>%
  pivot_wider(names_from = Genotype, values_from = Mean_Log2_Plus1_Polysome_Input_Ratio)

saveRDS(tpms_wide_mean, file = "TPMs/Polysome_Input_ratio_and_text.RDS")
  
