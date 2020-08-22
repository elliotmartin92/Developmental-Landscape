library(MGFR)
tpms = readRDS("raw_tpms_input.rds")
tpms_sums = rowSums(tpms[-1])
tpms_df = data.frame(row.names = tpms[[1]], tpms[-1])
tpms_exp_df = tpms_df[tpms_sums>10, ]
tpms_log_p1 = data.frame(row.names = tpms$FBGN, log2(tpms[-1]+1))
tpms_log_p1_expr = tpms_log_p1[tpms_sums>200,]

samples = c("BamHSbam", "BamHSbam", "BamHSbam", "BamHSbam", "BamRNAi",  "BamRNAi",  "BamRNAi", 
  "pelo_cyo", "pelo_cyo", "TKV", "TKV", "youngWT", "youngWT",  "BamHSbam",
  "BamHSbam", "BamHSbam", "BamHSbam", "BamRNAi",  "BamRNAi",  "BamRNAi",  "pelo_cyo",
  "pelo_cyo")

getMarkerGenes.rnaseq(data.mat = tpms_exp_df, class.vec = samples, score.cutoff = 0.2) 
