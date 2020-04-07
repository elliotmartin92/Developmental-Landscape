####generated og file name list to make conversion table####
original = list.files(path = "fastqs/", pattern = "*.gz")
original_df = data.frame(original_names = original)
original_df

# write.csv(original_df, "original_filenames.csv", quote = FALSE, row.names = FALSE)

#####read conversion table and change all filenames####
new_names = read.csv("fastqs/filename_conversion.csv", stringsAsFactors = FALSE)
new_names
new_filenames = paste0(new_names$standarized_name, ".fastq.gz")

setwd("fastqs")
# file.rename(from = new_names$original_names, to = new_filenames)


#####repeat same code for non-polysome ####
# original = list.files(path = "fastqs/nonPolysomeSeqFastqs", pattern = "*.gz")
# original_df = data.frame(original_names = original)
# original_df
# 
# write.csv(original_df, "original_filenames.csv", quote = FALSE, row.names = FALSE)

#####read conversion table and change all filenames####
new_names = read.csv("filename_conversion.csv", stringsAsFactors = FALSE)
new_names
new_filenames = paste0(new_names$standarized_name, ".fastq.gz")

# file.rename(from = new_names$original_names, to = new_filenames)

####fixing STAR output if you forgot to fix the input names####
#for dirs
dirnames_og = unlist(strsplit(new_names$original_names[25:28], ".fastq.gz"))
dirnames_new = new_names$standarized_name[25:28]
file.rename(from = dirnames_og, dirnames_new)
#for files in dirs
ogfiles = list.files(pattern = "_2")
newfiles = paste0("BamRNAi_input_2", unlist(strsplit(ogfiles, "_1"))[seq(2, 14, 2)])
file.rename(from = ogfiles, newfiles)
       