library(tidyverse)
library(stringr)

report <- read_lines("day_2.txt")

reports_list <- report |> 
    str_split(" ") |> 
    map(.x = _, as.numeric)


safe <- function(x) {
    len <- length(x)-1
    diff <- numeric()
    for (i in 1:(length(x)-1)) {
        diff[i] <- x[i+1] - x[i]
    }
    within_3 <- 1 <= abs(diff) & abs(diff) <= 3
    same_direct <- sum(diff >= 0) == len | sum(diff <= 0) == len
    sum(within_3) == len & same_direct
}

# report_1 <- reports_list[[2]] # test
# safe(report_1) 

# For entire list

safe_reports <- map_lgl(reports_list, safe)

sum(safe_reports)

# Part 2

safe_pad <- function(x) {

    safe_level <- safe(x)

    if (!safe_level) {
        N <- length(x)
        permute <- list()
        for (j in 1:(N)) {
        permute[[j]] <- x[-j]
        }
        safe_level <- sum(map_lgl(permute, safe)) >= 1
    } 
    safe_level
}

safe_reports_2 <- map_lgl(reports_list, safe_pad)

sum(safe_reports_2)
