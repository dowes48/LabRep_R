setwd(r"(C:\Users\dowes\OneDrive\Projects\LabRep_R)")
rm(list=ls())
library(stringr)

iter  <- "11"
in_dir <- "AbLab_Rpts"
in_pattern <- "AbLabs_.+\\.prn"
out_dir <- "Output"
out_name <- str_c("RS_", iter, ".csv")

file_list <- list.files(path=in_dir, pattern=in_pattern, recursive=TRUE)
for(fname in file_list) {
  print(file.path(in_dir, fname))
}
