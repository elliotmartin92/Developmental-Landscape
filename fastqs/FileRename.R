#####generated og file name list to make conversion table####
# original = list.files(pattern = "*.gz")
# original_df = data.frame(original_names = original)
# original_df
# 
# write.csv(original_df, "original_filenames.csv", quote = FALSE, row.names = FALSE)

#####read conversion table and change all filenames####
new_names = read.csv("filename_conversion.csv", stringsAsFactors = FALSE)
new_names
new_filenames = paste0(new_names$standarized_name, ".fastq.gz")

# file.rename(from = new_names$original_names, to = new_filenames)
