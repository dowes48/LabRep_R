rm(list=ls())
start_time <- Sys.time()
library(stringr)

TARGETDIR <- "AbLab_Rpts"
in_pattern <- "AbLabs_.+\\.prn"

form_feed <- '\f'
form_feed_count <- 0
line_count <- 0

process_lines <- function(lines){
    for (line in lines) {
        line_count <<- line_count + 1
        if(str_detect(line,form_feed)) {
            form_feed_count <<- form_feed_count + 1
        }
    }
}

file_list <- list.files(path=TARGETDIR, pattern=in_pattern, recursive=TRUE)
for(fname in file_list) {
    in_file <- file(file.path(TARGETDIR,fname), "r")
    in_lines_list <- readLines(in_file)
    close(in_file)
    process_lines(in_lines_list)
}

print(line_count)
print(form_feed_count)

end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)
