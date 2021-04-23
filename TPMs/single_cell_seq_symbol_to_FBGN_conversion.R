library(tidyverse)
library(openxlsx)

convert_sc_symbol_to_FBGN = function(sheet_name){
  conversion_table = read.xlsx("TPMs/Symbol_to_FBID_table.xlsx")
  head(conversion_table)
  single_cell_data = read.xlsx("TPMs/SC_seq_expression.xlsx", sheet = sheet_name)
  head(single_cell_data)
  
  single_cell_data_converted = left_join(x = single_cell_data, y = conversion_table, by = c("X1"="submitted_item")) %>% 
    relocate(FBGN = validated_id) %>% 
    rename(Symbol=X1) %>% 
    select(-current_symbol)
  head(single_cell_data_converted)
  addWorksheet(wb, sheetName = sheet_name)
  writeData(wb, sheet_name, single_cell_data_converted)
}

wb = createWorkbook()
sheets_names = getSheetNames("TPMs/SC_seq_expression.xlsx")
lapply(sheets_names, convert_sc_symbol_to_FBGN)
saveWorkbook(wb, file = "TPMs/SC_seq_expression_FBID.xlsx", overwrite = TRUE) #save workbook
