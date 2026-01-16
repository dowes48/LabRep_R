setwd(r"(C:\Users\dowes\OneDrive\Projects\LabRep_R)")
rm(list=ls())
library(stringr)
start_time <- Sys.time()

iter  <- "14"
in_dir <- "AbLab_Rpts"
in_pattern <- "AbLabs_.+\\.prn"
out_dir <- "Output"
out_name <- str_c("R_version_", iter, ".csv")
out_fullName <- file.path(out_dir, out_name)

clean_lab_vals <- list(ticket="", d_coll="", age="", sex="", d_perf="", face="",
                       gluc="", fruct="", a1c="", bun="",creat="", alkp="", 
                       bil_t="", ast="", alt="", ggt="", prot="", albu="",
                       glob="", chol="", hdl="", tch_r="", trigs="", u_pro="",
                       u_cre="", u_pcr="", bmi="", pulse="", bpsys="", bpdias="")
lab_vals <- clean_lab_vals

clean_ID_vals <- list(ticket="", d_coll="", name="", dob="", sex="", ssn="", zip="")
ID_vals <- clean_ID_vals

get_values <- function(line) {
    if(str_detect(line,"NAME")) {
        ID_vals$name <<- str_trim(str_sub(line, start=6, end=-1), side="both")
        return()
    }
    else if(str_detect(line,"DOB/SEX:")){
        lab_vals$sex <<- str_sub(line, start=21, end=21)
        ID_vals$sex <<- lab_vals$sex
        ID_vals$zip <<- str_trim(str_sub(line, start=60, end=-1), side="both")
        return()
    }
    else if(str_detect(line,"SOC SEC NO:")){
        lab_vals$d_coll <<- str_sub(line, start=55, end=64)
        ID_vals$d_coll <<- lab_vals$ticket
        ID_vals$ssn <<- str_sub(line, start=12, end=21)
        return()
    }
    else if(str_detect(line,"TICKET NUMBER")){
        lab_vals$ticket <<- str_sub(line, start=15, end=25)
        ID_vals$ticket <<- lab_vals$ticket
        lab_vals$d_perf <<- str_sub(line, start=55, end=64)
        return()
    }
    
    
    else if(str_detect(line,"GLUCOSE ")){
        lab_vals$gluc <<- str_trim(str_sub(line, start=30, end=40), side="both")
    }
    else if(str_detect(line,"\f")){
        tempStr <- str_c(lab_vals, collapse=",")
        writeLines(tempStr, con=file_out)
        lab_vals <<- clean_lab_vals
        ID_vals <<- clean_ID_vals
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

end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)

