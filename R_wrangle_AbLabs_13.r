setwd(r"(C:\Users\dowes\OneDrive\Projects\LabRep_R)")
rm(list=ls())
library(stringr)

iter  <- "13"
in_dir <- "AbLab_Rpts/AbLab_2021"
in_pattern <- "AbLabs_.+\\.prn"
out_dir <- "Output"
out_name <- str_c("RStudio_", iter, ".csv")
out_fullName <- file.path(out_dir, out_name)

clean_lab_vals <- list(name="", tickNo="", gluc="")
lab_vals <- clean_lab_vals

get_values <- function(line) {
    if(str_detect(line,"NAME")) {
        lab_vals$name <<- str_trim(str_sub(line, start=6, end=-1), side="both")
        return()
    }
    else if(str_detect(line,"TICKET NUMBER")){
        lab_vals$tickNo <<- str_trim(str_sub(line, start=15, end=25), side="both")
        return()
    }
    else if(str_detect(line,"GLUCOSE ")){
        lab_vals$gluc <<- str_trim(str_sub(line, start=30, end=40), side="both")
    }
    else if(str_detect(line,"\f")){
        tempStr <- str_c(lab_vals, collapse=",")
        writeLines(tempStr, con=file_out)
        lab_vals <<- clean_lab_vals
        return()
    }
    else return()
}

process_lines <- function(lines){
    for (line in lines) {
        get_values(line)
        }
}

#*************************************************************

file_out <- file(out_fullName,"w")
temp_Str <- "name,ticket,gluc"
writeLines(temp_Str, con=file_out)
file_list <- list.files(path=in_dir, pattern=in_pattern, recursive=TRUE)
for(fname in file_list) {
    in_file <- file(file.path(in_dir, fname), "r")
    in_lines_list <- readLines(in_file)
    close(in_file)
    process_lines(in_lines_list)
}
close(file_out)
