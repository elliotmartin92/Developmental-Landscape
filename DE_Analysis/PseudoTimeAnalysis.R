library(phenopath)
sce <- SummarizedExperiment(assays = list(exprs = picked_names), 
                            colData = allseq)
