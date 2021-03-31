changing_genes = readRDS("developmentally_regulated_gene_list.RDS")
modls = function(x){log2(x+1)}
heat.data = data.seq %>%
  filter(FBGN %in% changing_genes) %>%
  dplyr::select(MeanTPM_TKV_input,
                MeanTPM_BamRNAi_input,
                MeanTPM_BamHSbam_input,
                MeanTPM_youngWT_input) %>% 
  modls() %>%
  data.frame()
rownames(heat.data) = data.seq %>% filter(FBGN %in% changing_genes) %>% pull(FBGN)
heat <<- heatmaply(heat.data,
                 showticklabels = c(TRUE, FALSE),
                 seriate = "none")