library(data.table)
library(stringr)
library(tidyverse)

# This script computes TPMs for all libraries and computes polysome/input ratio

temp = list.files(path = "fastqs/STAR_processed_files/", pattern = "*ReadsPerGene.out.tab", recursive = TRUE)
data.table(temp)
pick = temp[] #pick or reorder featurecounts
data.table(pick)
temp_names = unlist(strsplit(pick, 'ReadsPerGene.out.tab'))
picked_names = unlist(strsplit(temp_names, '/'))[seq(2, length(temp_names)*2, 2)]
picked_names = str_replace(string = picked_names, pattern = "pelo_cyo", replacement = "peloCyo")
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

featLens = read.table(file = "TPMs/Feature_lengths_dm6ensgene.tabular",  
                      header = TRUE, 
                      stringsAsFactors = FALSE)

tpms_all = data.table(apply(X = allseq, 2, tpm, lengths=featLens[[2]]))
rownames(tpms_all) = rownames(allseq)
tpms_all_tib = as_tibble(data.frame(FBGN = rownames(tpms_all), tpms_all))
head(tpms_all_tib)
# write_rds(tpms_all_tib, "ShinyExpresionMap/raw_tpms_all.rds")

tpms_all_long = tpms_all_tib %>% 
  pivot_longer(
    cols = -FBGN,
    names_to = c("Genotype", "Source", "Replicate"),
    names_sep  = "_",
    values_to = "TPM"
)

std <- function(x) sd(x)/sqrt(length(x))

tpms_long_mean = tpms_all_long %>% 
  group_by(Genotype, Source, FBGN) %>% 
  summarise(MeanTPM = mean(TPM), se = std(TPM))

tpms_long_mean$MeanTPMpmError = paste0(round(signif(tpms_long_mean$MeanTPM, digits = 3), digits = 1), "±", 
                                       round(signif(tpms_long_mean$se, digits = 3), digits = 1))

tpms_long_mean$Genotype = factor(tpms_long_mean$Genotype, 
                              levels = c("TKV", "BamRNAi", "BamHSbam", "youngWT", "peloCyo"))

tpms_wide_mean = tpms_long_mean %>% 
  arrange(Genotype) %>% 
  dplyr::select(-se) %>% 
  pivot_wider(names_from = c(Genotype, Source), values_from = c(MeanTPM, MeanTPMpmError))

# saveRDS(tpms_wide_mean, file = "TPMs/All_Mean_TPMs_and_text.RDS")


tpms_all_long

tpms_all_long$Genotype = factor(tpms_all_long$Genotype, 
                                levels = c("TKV", "BamRNAi", "BamHSbam", "youngWT", "peloCyo"))

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

# saveRDS(tpms_wide_mean, file = "TPMs/Polysome_Input_ratio_and_text.RDS")

rp = read.xlsx("../RibosomeBiogen/PolysomeSeq/RPs.xlsx")

test = 
mean_log2_polysome_input_ratio %>% 
  filter(FBGN %in% rp[[1]]) %>%
  pivot_longer(cols = -FBGN, names_to = "Genotype", values_to = "TE")

test$Genotype = factor(test$Genotype, 
                                levels = c("TKV", "BamRNAi", "BamHSbam", "youngWT", "peloCyo"))

ggplot(test, aes(Genotype, TE))+
  geom_boxplot()+
  geom_point()

tpms_wide_mean$color = "nontarget"
tpms_wide_mean$color[tpms_wide_mean$FBGN %in% rp[[1]]] = "RP"
tpms_wide_mean$color = factor(tpms_wide_mean$color, levels = c("RP", "nontarget"))

tpms_wide_mean %>% 
  arrange(color) %>% 
  ggplot(aes(x=log2(MeanTPM_youngWT_input+1), y=log2(MeanTPM_youngWT_polysome)+1), color=color)+
  geom_point()+
  scale_color_manual(values = c("nontarget" = "black", 
                                "RP" = "darkred"))+
  xlim(c(0,16))+
  ylim(c(0,16))
  
