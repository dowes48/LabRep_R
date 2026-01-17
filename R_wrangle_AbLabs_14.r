setwd(r"(C:\Users\dowes\OneDrive\Projects\LabRep_R)")
rm(list=ls())
library(stringr)
start_time <- Sys.time()

iter  <- "14"
in_dir <- "AbLab_Rpts/AbLab_2018-20"
in_pattern <- "AbLabs_.+\\.prn"
out_dir <- "Output"

lab_out_name <- str_c("Labresults_", iter, ".csv")
lab_out_fullName <- file.path(out_dir, lab_out_name)

ID_out_name <- str_c("IDresults_", iter, ".csv")
ID_out_fullName <- file.path(out_dir, ID_out_name)

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
        date_OB <- str_sub(line, start=9, end=19)
        ID_vals$dob <<- date_OB
        return()
    }
    else if(str_detect(line,"SOC SEC NO:")){
        date_coll <- str_sub(line, start=55, end=65)
        lab_vals$d_coll <<- date_coll
        ID_vals$d_coll <<- date_coll
        ID_vals$ssn <<- str_sub(line, start=12, end=21)
        return()
    }
    else if(str_detect(line,"TICKET NUMBER")){
        lab_vals$ticket <<- str_sub(line, start=15, end=25)
        ID_vals$ticket <<- lab_vals$ticket
        lab_vals$d_perf <<- str_sub(line, start=55, end=65)
        return()
    }
    
    else if(str_detect(line,"GLUCOSE ")){
        lab_vals$gluc <<- str_trim(str_sub(line, start=30, end=40), side="both")
        return()
    }
    else if(str_detect(line,"\f")){
        tempStr <- str_c(lab_vals, collapse=",")
        writeLines(tempStr, con=lab_out)
        lab_vals <<- clean_lab_vals
        
        tempStr <- str_c(ID_vals, collapse=",")
        writeLines(tempStr, con=ID_out)
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

lab_out <- file(lab_out_fullName,"w")
ID_out <- file(ID_out_fullName,"w")

temp_Str <- "ticket, gluc\n"
writeLines(temp_Str, con=lab_out)

temp_Str <- "ticket, name\n"
writeLines(temp_Str, con=ID_out)

file_list <- list.files(path=in_dir, pattern=in_pattern, recursive=TRUE)
for(fname in file_list) {
    in_file <- file(file.path(in_dir, fname), "r")
    in_lines_list <- readLines(in_file)
    close(in_file)
    process_lines(in_lines_list)
}

close(lab_out)
close(ID_out)

end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)

