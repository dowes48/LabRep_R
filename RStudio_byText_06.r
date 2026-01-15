setwd("D:/Publications/TextData/examples/LabReports/RStudio")
rm(list=ls())

iter  <- "06"
in_dir <- "../Dev_data"
in_pattern <- "Labs.+\\.txt"
out_dir <- "../Output"
out_name <- paste0("RStudio_", iter, ".txt")

ptm <- proc.time()

lr <<- list(name  ="", 
            dob   ="", 
            sex   ="",
            zip   ="",
            ssn   ="",
            d_coll="",
            ticket="",
            d_perf="",
            face  ="",
            gluc  ="", 
            fruct ="",
            a1c   ="",
            bun   ="",
            creat ="",
            alkp  ="",
            bil_t ="",
            ast   ="",
            alt   ="",
            ggt   ="",
            prot  ="",
            albu  ="",
            glob  ="",
            chol  ="",
            hdl   ="",
            tch_r ="",
            trigs ="",
            u_pro ="",
            u_cre ="", 
            u_pcr ="",
            bmi   ="",
            pulse ="",
            bp    ="")

pr <<- list(has_data=0, line="", out_fh="")
pr$lr <- lr

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)

proc_line_name <- function(){
  if (substr(pr$line, 1, 6)!= "NAME: ") return(0)
  pr$lr$name <<- trim(substr(pr$line, 7, 28))
  pr$has_data <<- 1
  return(1)
}

proc_line_dob <- function(){
  if (substr(pr$line, 1, 9)!= "DOB/SEX: ") return(0)
  pr$lr$dob <<- substr(pr$line, 10, 19)
  pr$lr$sex <<- substr(pr$line, 22, 22)
  pr$lr$zip <<- substr(pr$line, 61, 65)
  pr$has_data <<- 1
  return(1)
}

proc_line_ssn <- function(){
  if (substr(pr$line, 1, 12)!= "SOC SEC NO: ") return(0)
  pr$lr$ssn <<- substr(pr$line, 13, 21)
  pr$lr$d_coll <<- substr(pr$line, 56, 65)
  pr$has_data <<- 1
  return(1)
}

proc_line_tick <- function(){
  if (substr(pr$line, 1, 15)!= "TICKET NUMBER: ") return(0)
  pr$lr$ticket <<- substr(pr$line, 15, 24)
  pr$lr$d_perf <<- substr(pr$line, 56, 65)
  pr$has_data <<- 1
  return(1)
}

proc_line_face <- function(){
  if (substr(pr$line, 1, 10)!= "TYPE/AMT: ") return(0)
  pr$lr$face <<- trim(substr(pr$line, 23, 32))
  pr$has_data <<- 1
  return(1)
}

proc_line_gluc <- function(){
  if (substr(pr$line, 1, 15)!= "GLUCOSE (MG/DL)") return(0)
  pr$lr$gluc <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_fruct <- function(){
  if (substr(pr$line, 1, 15)!= "FRUCTOSAMINE (M") return(0)
  pr$lr$fruct <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_a1c <- function(){
  if (substr(pr$line, 1, 15)!= "HB A1C (%)     ") return(0)
  pr$lr$a1c <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_bun <- function(){
  if (substr(pr$line, 1, 15)!= "BUN (MG/DL)    ") return(0)
  pr$lr$bun <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_creat <- function(){
  if (substr(pr$line, 1, 15)!= "CREATININE (MG/") return(0)
  pr$lr$creat <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_alkp <- function(){
  if (substr(pr$line, 1, 15)!= "ALK. PHOS. (U/L") return(0)
  pr$lr$alkp <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_bil_t <- function(){
  if (substr(pr$line, 1, 15)!= "BILI. TOT. (MG/") return(0)
  pr$lr$bil_t <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_ast <- function(){
  if (substr(pr$line, 1, 15)!= "AST(SGOT) (U/L)") return(0)
  pr$lr$ast <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_alt <- function(){
  if (substr(pr$line, 1, 15)!= "ALT(SGPT) (U/L)") return(0)
  pr$lr$alt <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_ggt <- function(){
  if (substr(pr$line, 1, 15)!= "GGT(GGTP) (U/L)") return(0)
  pr$lr$ggt <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_prot <- function(){
  if (substr(pr$line, 1, 15)!= "TOT. PROTEIN (G") return(0)
  pr$lr$prot <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_albu <- function(){
  if (substr(pr$line, 1, 15)!= "ALBUMIN (G/DL) ") return(0)
  pr$lr$albu <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_glob <- function(){
  if (substr(pr$line, 1, 15)!= "GLOBULIN (G/DL)") return(0)
  pr$lr$glob <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_chol <- function(){
  if (substr(pr$line, 1, 15)!= "CHOLESTEROL (MG") return(0)
  pr$lr$chol <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_hdl <- function(){
  if (substr(pr$line, 1, 15)!= "HDL CHOLESTEROL") return(0)
  pr$lr$hdl <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_tch_r <- function(){
  if (substr(pr$line, 1, 15)!= "CHOL/HDL CHOL R") return(0)
  pr$lr$tch_r <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_trigs <- function(){
  if (substr(pr$line, 1, 15)!= "TRIGLYCERIDES (") return(0)
  pr$lr$trigs <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_u_pro <- function(){
  if (substr(pr$line, 1, 15)!= "URINE PROTEIN (") return(0)
  pr$lr$u_pro <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_u_cre <- function(){
  if (substr(pr$line, 1, 15)!= "URINE CREATININ") return(0)
  pr$lr$u_cre <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_u_pcr <- function(){
  if (substr(pr$line, 1, 15)!= "PROT/CREAT RATI") return(0)
  pr$lr$u_pcr <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_bmi <- function(){
  if (substr(pr$line, 1, 15)!= "BMI            ") return(0)
  pr$lr$bmi <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_pulse <- function(){
  if (substr(pr$line, 1, 15)!= "PULSE          ") return(0)
  pr$lr$pulse <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_bp <- function(){
  if (substr(pr$line, 1, 15)!= "BLOOD PRESSURE ") return(0)
  pr$lr$bp <<- trim.leading(substr(pr$line, 32, 38))
  pr$has_data <<- 1
  return(1)
}

proc_line_FF <- function(){
  if (substr(pr$line, 1, 4)!= "<FF>") return(0)
  if (pr$has_data==1) {
    tempStr <- paste(pr$lr, collapse="|")
    writeLines(tempStr, con=pr$out_fh)
    pr$lr <<- lr
    pr$has_data <<- 0
  }
  return(1)
}
processLine <- function(){
  if (proc_line_name())   return()
  if (proc_line_dob()) 		return()
  if (proc_line_ssn())		return()
  if (proc_line_tick())		return()
  if (proc_line_face())		return()
  if (proc_line_gluc())		return()
  if (proc_line_fruct())	return()
  if (proc_line_a1c())		return()
  if (proc_line_bun())		return()
  if (proc_line_creat())	return()
  if (proc_line_alkp())		return()
  if (proc_line_bil_t())	return()
  if (proc_line_ast())		return()
  if (proc_line_alt())		return()
  if (proc_line_ggt())		return()
  if (proc_line_prot())		return()
  if (proc_line_albu())		return()
  if (proc_line_glob())		return()
  if (proc_line_chol())		return()
  if (proc_line_hdl())		return()
  if (proc_line_tch_r())	return()
  if (proc_line_trigs())	return()
  if (proc_line_u_pro())	return()
  if (proc_line_u_cre())	return()
  if (proc_line_u_pcr())	return()
  if (proc_line_bmi())		return()
  if (proc_line_pulse())	return()
  if (proc_line_bp())		  return()
  if (proc_line_FF())  	  return()
}

processFile <- function(lines){
  for (line in lines) {
    pr$line <<- line
    processLine()
  }
}

#********** "program" starts here ***************
pr$out_fh <- file(file.path(out_dir, out_name), "w")
temp <- "has_data|name|dob|sex|zip|ssn|d_coll|ticket|d_perf|face|gluc|fruct|a1c|bun|creat|alkp|bil_t|ast|alt|ggt|prot|albu|glob|chol|hdl|tch_r|trigs|u_pro|u_cre|u_pcr|bmi|pulse|bpsys|bpdias"
writeLines(temp, con=pr$out_fh)
flist <- list.files(path=in_dir, pattern=in_pattern, recursive=TRUE)
for (fl in flist) {
  in_name  <- file.path(in_dir, fl)
  in_fh  <- file(in_name, "r")
  lines  <- readLines(in_fh)
  close(in_fh)
  processFile(lines)
}
close(pr$out_fh)

print(proc.time() - ptm)
