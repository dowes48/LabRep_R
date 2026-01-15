setwd(r"(C:\Users\dowes\OneDrive\Projects\LabRep_R)")
rm(list=ls())
library(stringr)

iter  <- "13"
in_dir <- "AbLab_Rpts/AbLab_2021"
in_pattern <- "AbLabs_.+\\.prn"
out_dir <- "Output"
out_name <- str_c("RS_", iter, ".csv")

get_values <- function(line) {
    if(str_detect(line,"NAME")) {
        print(str_sub(line, start=6, end=-1))
        return()
    }
    else if(str_detect(line,"TICKET NUMBER:")){
        print(str_sub(line, start=15, end=25))
    }
    else return()
}

process_lines <- function(lines){
    for (line in lines) {
        get_values(line)
        }
}

file_list <- list.files(path=in_dir, pattern=in_pattern, recursive=TRUE)
for(fname in file_list) {
    in_file <- file(file.path(in_dir, fname), "r")
    in_lines_list <- readLines(in_file)
    close(in_file)
    process_lines(in_lines_list)
}
