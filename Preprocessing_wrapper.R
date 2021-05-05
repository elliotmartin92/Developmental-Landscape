# Preprocessing wrapper
source(file = "Normalized_expression/single_cell_seq_symbol_to_FBGN_conversion.R")

files.sources = list.files("ShinyExpresionMap/Preprocessing_scripts", include.dirs = TRUE)
sapply(files.sources, source)