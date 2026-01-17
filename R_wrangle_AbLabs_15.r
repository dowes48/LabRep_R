setwd(r"(C:\Users\dowes\OneDrive\Projects\LabRep_R)")
rm(list=ls())
library(stringr)
#start_time <- Sys.time()

iter  <- "15"
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
                       u_crea="", u_pcr="", bmi="", pulse="", bpsys="", bpdias="")
lab_vals <- clean_lab_vals

clean_ID_vals <- list(ticket="", d_coll="", name="", dob="", sex="", ssn="", zip="")
ID_vals <- clean_ID_vals

fix_missing_vals <- function(val_str){
    temp_Str <-str_trim(val_str, side="both")
    if(temp_Str=="NVG"){return("9998")}
    else if(temp_Str=="NVH"){return("9999")}
    else {return(temp_Str)}
}
get_values <- function(line) {
    if(str_detect(line,"NAME")) {
        ID_vals$name <<- str_trim(str_sub(line, start=6, end=-1), side="both")
        return()
    } else if(str_detect(line,"DOB/SEX:")){
        lab_vals$sex <<- str_sub(line, start=21, end=21)
        ID_vals$sex <<- lab_vals$sex
        ID_vals$zip <<- str_trim(str_sub(line, start=60, end=-1), side="both")
        date_OB <- str_sub(line, start=9, end=19)
        ID_vals$dob <<- date_OB
        return()
    } else if(str_detect(line,"SOC SEC NO:")){
        date_coll <- str_sub(line, start=55, end=65)
        lab_vals$d_coll <<- date_coll
        ID_vals$d_coll <<- date_coll
        ID_vals$ssn <<- str_sub(line, start=12, end=21)
        return()
    } else if(str_detect(line,"TICKET NUMBER")){
        lab_vals$ticket <<- str_sub(line, start=15, end=25)
        ID_vals$ticket <<- lab_vals$ticket
        lab_vals$d_perf <<- str_sub(line, start=55, end=65)
        return()
    } else if(str_detect(line,"GLUCOSE ")){
        lab_vals$gluc <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"FRUCTOSAMINE  ")){
        lab_vals$fruct <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"HB A1C (%)  ")){
        lab_vals$a1c <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"BUN (MG/DL) ")){
        lab_vals$bun <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"CREATININE ")){
        lab_vals$creat <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"ALK. PHOS. ")){
        lab_vals$alkp <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"BILI\\. TOT\\. ){
        lab_vals$bil_t <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"AST\\(SGOT\\) ")){
        lab_vals$ast <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"ALT\\(SGPT\\) ")){
        lab_vals$alt <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"GGT\\(GGTP\\) ")){
        lab_vals$ggt <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"TOT\\. PROTEIN ")){
        lab_vals$prot <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"ALBUMIN ")){
        lab_vals$alb <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"GLOBULIN ")){
        lab_vals$glob <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"CHOLESTEROL ")){
        lab_vals$chol <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"HDL CHOLESTERO")){
        lab_vals$hdl <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"CHOL/HDL CHOL ")){
        lab_vals$tch_r <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"TRIGLYCERIDES ")){
        lab_vals$trigs <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"URINE PROTEIN ")){
        lab_vals$u_pro <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"URINE CREATINI")){
        lab_vals$u_crea <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"PROT/CREAT RAT")){
        lab_vals$u_pcr <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"BMI  ")){
        lab_vals$bmi <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"PULSE         ")){
        lab_vals$pulse <<- fix_missing_vals(str_sub(line, start=30, end=40))
        return()
    } else if(str_detect(line,"BLOOD PRESSURE")){
        lab_vals$bpsys <<-  str_sub(line, start=32, end=34)
        lab_vals$bpdias <<- str_sub(line, start=36, end=37)
        return()

        
    } else if(str_detect(line,"\f")){
        tempStr <- str_c(lab_vals, collapse=",")
        writeLines(tempStr, con=lab_out)
        lab_vals <<- clean_lab_vals
        
        tempStr <- str_c(ID_vals, collapse=",")
        writeLines(tempStr, con=ID_out)
        ID_vals <<- clean_ID_vals
        return()
    } else return()
}

process_lines <- function(lines){
    for (line in lines) {
        get_values(line)
        }
}

#*************************************************************

lab_out <- file(lab_out_fullName,"w")
ID_out <- file(ID_out_fullName,"w")

temp_Str <- str_c("ticket","d_coll","age","sex","d_perf","face","gluc","fruct",
            "a1c","bun,creat","alkp","bil_t","ast","alt","ggt","prot","albu","glob", 
            "chol","hdl","tch_r","trigs","u_pro","u_crea","u_pcr","bmi","pulse","bpsys","bpdias",
            sep=",")
writeLines(temp_Str, con=lab_out)

temp_Str <- "ticket, d_coll, name, dob, sex, ssn, zip"
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

#end_time <- Sys.time()
#time_taken <- end_time - start_time
#print(time_taken)

