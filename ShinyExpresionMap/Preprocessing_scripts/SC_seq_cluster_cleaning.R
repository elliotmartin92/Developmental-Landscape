library(tidyverse)
library(openxlsx)

convert_sc_symbol_to_FBGN = function(sheet_name){
  # Conversion table from Maija
  conversion_table = read_tsv("Normalized_expression/Symbol_to_FBID_table_sc_seq.tsv", col_names = c("FBGN", "Symbol"))
  # Clusterwise expression data
  single_cell_data = read.xlsx("Normalized_expression/SC_seq_expression.xlsx", sheet = sheet_name)
  
  # Clean up names and generate bins
  single_cell_data_converted = left_join(x = single_cell_data, y = conversion_table, by = c("X1"="Symbol")) %>%  
    rename(Symbol=X1) %>% 
    relocate(FBGN, .before = Symbol) %>% 
    pivot_longer(-c(FBGN, Symbol), names_to = "Stage", values_to = "Expression") %>% 
    dplyr::group_by(Symbol, Stage) %>% 
    mutate(bin = cut(as.numeric(Expression), 
                     breaks = c(-1,0.05,0.25,0.5,2.5,25,200), 
                     labels=c("None","Very Low","Low","Med","High","Very High"))) %>% 
    pivot_wider(id_cols = c(FBGN, Symbol), names_from = Stage, values_from = c(Expression, bin))
    
  # save results as RDS object
  write_rds(single_cell_data_converted, 
            paste0("ShinyExpresionMap/Preprocessed_data/preprocessed_single_cell_seq_data_", sheet_name,".RDS"))
}

# apply convert_sc_symbol_to_FBGN to each sheet of single cell data
sheets_names = getSheetNames("Normalized_expression/SC_seq_expression.xlsx")
lapply(sheets_names, convert_sc_symbol_to_FBGN)
