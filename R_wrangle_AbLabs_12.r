setwd(r"(C:\Users\dowes\OneDrive\Projects\LabRep_R)")
rm(list=ls())
library(stringr)

iter  <- "12"
in_dir <- "AbLab_Rpts/AbLab_2021"
in_pattern <- "AbLabs_.+\\.prn"
out_dir <- "Output"
out_name <- str_c("RS_", iter, ".csv")

process_lines <- function(lines){
  for (line in lines) {
    print(line)
  }
}

file_list <- list.files(path=in_dir, pattern=in_pattern, recursive=TRUE)
for(fname in file_list) {
  in_file <- file(file.path(in_dir, fname), "r")
  in_lines_list <- readLines(in_file)
  close(in_file)
  process_lines(in_lines_list)
}
