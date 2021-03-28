library(GO.db)
library(org.Dm.eg.db)

all_GO_array = Term(GOTERM)
all_GO_tibble = tibble(GOID = names(test), description=test)
write_tsv(all_GO_tibble, "Preprocessed_data/all_go_terms.tsv")

GO_to_entrez = as.data.frame(org.Dm.egGO2ALLEGS)

entrez_to_FBID = as.data.frame(org.Dm.egENSEMBL)

GO_master_df = merge(GO_to_entrez, entrez_to_FBID, by = "gene_id")

GO_stripped_df = GO_master_df[-c(1,3)]

write_rds(GO_stripped_df, "Preprocessed_data/GO_Term_to_FBID.rds")
