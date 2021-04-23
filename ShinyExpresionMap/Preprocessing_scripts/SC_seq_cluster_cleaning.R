library(tidyverse)
library(openxlsx)

GC_clusters = read.xlsx("TPMs/SC_seq_expression.xlsx", sheet = 1)
head(GC_clusters)
names(GC_clusters)[names(GC_clusters) == "X1"] = "symbol"

library(org.Dm.eg.db)
symbol_to_fbgn =  function(fbid){
  AnnotationDbi::select(org.Dm.eg.db, fbid, 
                        columns=c("FLYBASE"), 
                        keytype="SYMBOL")
}

# GC_clusters$FBGN = symbol_to_fbgn(GC_clusters$symbol)$FLYBASE

GC_preprocessed =
GC_clusters %>% 
  pivot_longer(-c(symbol, FBGN), names_to = "Stage", values_to = "Expression") %>% 
  dplyr::group_by(symbol, FBGN, Stage) %>% 
  mutate(bin = cut(as.numeric(Expression), 
         breaks = c(-1,0.05,0.25,0.5,2.5,25,200), 
         labels=c("None","Very Low","Low","Med","High","Very High"))) %>% 
  pivot_wider(id_cols = symbol, names_from = Stage, values_from = c(Expression, bin))
head(GC_preprocessed)

conversion_table = read.xlsx("TPMs/Symbol_to_FBID_table.xlsx")
GC_preprocessed_fbgn = left_join(x = GC_preprocessed, 
                                 y = conversion_table, 
                                 by = c("symbol"="submitted_item")) %>% 
  dplyr::rename(FBGN=validated_id) %>% 
  dplyr::select(-current_symbol)

write_rds(GC_preprocessed, "ShinyExpresionMap/Preprocessed_data/preprocessed_single_cell_seq_data.RDS")
