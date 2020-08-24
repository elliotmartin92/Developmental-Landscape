library(GO.db)
all_GO_array = Term(GOTERM)
all_GO_tibble = tibble(GOID = names(test), description=test)
write_tsv(all_GO_tibble, "Preprocessed_data/all_go_terms.tsv")
