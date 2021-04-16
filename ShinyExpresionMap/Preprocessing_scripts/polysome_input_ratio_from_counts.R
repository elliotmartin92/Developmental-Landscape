library(data.table)
library(stringr)
library(tidyverse)
library(org.Dm.eg.db)
library(annotate)

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

polysome_ratios = tpms_all_long %>% 
  group_by(FBGN, Genotype, Replicate) %>% 
  mutate(polysome_over_input = (TPM+1)/(TPM[Source=="input"]+1)) %>% 
  mutate(log2_polysome_over_input = log2(polysome_over_input)) %>% 
  filter(Source == "polysome")

std <- function(x) sd(x)/sqrt(length(x))

polysome_ratios_mean = polysome_ratios %>% 
  group_by(FBGN, Genotype, Source) %>% 
  summarise(Mean_polysome_over_input = mean(polysome_over_input), 
            Mean_polysome_over_input_error = std(polysome_over_input),
            Mean_log2_polysome_over_input = mean(log2_polysome_over_input), 
            Mean_log2_polysome_over_input_error = std(log2_polysome_over_input))

polysome_ratios_mean$log2MeanRatioError = paste0(round(signif(polysome_ratios_mean$Mean_log2_polysome_over_input, digits = 3), 
                                                     digits = 1), "±", 
                                              round(signif(polysome_ratios_mean$Mean_log2_polysome_over_input_error, digits = 3), 
                                                    digits = 1))

polysome_ratios_mean$Genotype = factor(polysome_ratios_mean$Genotype, 
                              levels = c("TKV", "BamRNAi", "BamHSbam", "youngWT"))

polysome_ratios_mean_wide = polysome_ratios_mean %>% 
  select(-Mean_polysome_over_input, -Mean_polysome_over_input_error, -Source) %>% 
  arrange(Genotype) %>% 
  pivot_wider(names_from = Genotype, values_from = c(Mean_log2_polysome_over_input,
                                                     Mean_log2_polysome_over_input_error,
                                                     log2MeanRatioError))

saveRDS(polysome_ratios_mean_wide, file = "TPMs/Polysome_Input_ratio_and_text.RDS")

fbgn_to_symbol =  function(fbid){
  AnnotationDbi::select(org.Dm.eg.db, fbid, 
                        columns=c("SYMBOL"), 
                        keytype="FLYBASE")
}

polysome_ratios_mean_wide$FBGN = as.character(polysome_ratios_mean_wide$FBGN)
Symbol = fbgn_to_symbol(polysome_ratios_mean_wide$FBGN)[[2]]
polysome_ratios_mean_wide$symbol = Symbol

polysome_ratios_mean_wide$TKVbin1 = cut(as.numeric(polysome_ratios_mean_wide$Mean_log2_polysome_over_input_TKV), 
                                        breaks = c(-11,-0.5,-0.2,0,0.2,0.5,11), 
                                        labels=c("None","Very Low","Low","Med","High","Very High"))

polysome_ratios_mean_wide$Bambin1 = cut(as.numeric(polysome_ratios_mean_wide$Mean_log2_polysome_over_input_BamRNAi), 
                                        breaks = c(-11,-0.5,-0.2,0,0.2,0.5,11), 
                                        labels=c("None","Very Low","Low","Med","High","Very High"))

polysome_ratios_mean_wide$Cystbin1 = cut(as.numeric(polysome_ratios_mean_wide$Mean_log2_polysome_over_input_BamHSbam), 
                                         breaks = c(-11,-0.5,-0.2,0,0.2,0.5,11), 
                                         labels=c("None","Very Low","Low","Med","High","Very High"))

polysome_ratios_mean_wide$Virginbin1 = cut(as.numeric(polysome_ratios_mean_wide$Mean_log2_polysome_over_input_youngWT), 
                                           breaks = c(-11,-0.5,-0.2,0,0.2,0.5,11), 
                                           labels=c("None","Very Low","Low","Med","High","Very High"))

saveRDS(polysome_ratios_mean_wide, "ShinyExpresionMap/Preprocessed_data/preprocessed_polysome_seq_data.RDS", compress = TRUE)
