library(tidyverse)
library(openxlsx)

convert_sc_symbol_to_FBGN = function(sheet_name){
  conversion_table = read_tsv("Normalized_expression/Symbol_to_FBID_table_sc_seq.tsv", col_names = c("FBGN", "Symbol"))
  head(conversion_table)
  single_cell_data = read.xlsx("Normalized_expression/SC_seq_expression.xlsx", sheet = sheet_name)

  single_cell_data_converted = left_join(x = single_cell_data, y = conversion_table, by = c("X1"="Symbol")) %>%  
    rename(Symbol=X1) %>% 
    relocate(FBGN, .before = Symbol)

  addWorksheet(wb, sheetName = sheet_name)
  writeData(wb, sheet_name, single_cell_data_converted)
}

wb = createWorkbook()
sheets_names = getSheetNames("Normalized_expression/SC_seq_expression.xlsx")
lapply(sheets_names, convert_sc_symbol_to_FBGN)
saveWorkbook(wb, file = "Normalized_expression/SC_seq_expression_FBID.xlsx", overwrite = TRUE) #save workbook

