library(tidyverse)

mem_string <- read_file("day_3.txt")

mul_list <- str_extract_all(mem_string, "(?<=mul\\()\\d*,\\d*(?=\\))") |> unlist()

x <- mul_list[1]

multiplication_from_string <- function(x) {
  vals <- as.numeric(unlist(str_split(x, ",")))
  vals[1] * vals[2]
}

all_vals <- map_dbl(mul_list, multiplication_from_string)  

sum(all_vals)


instruct_list <- str_extract_all(mem_string, "(?<=mul\\()\\d*,\\d*(?=\\))|do(?=\\(\\))|don't(?=\\(\\))") |> unlist()

do <- TRUE

values <- numeric()

for (i in seq_along(instruct_list)) {
  if (do) {
    if (instruct_list[i] == "don't") {
      do <- FALSE
      values[i] <- 0
    } else if (instruct_list[i] == "do") {
      values[i] <- 0
    } else
    values[i] <- multiplication_from_string(instruct_list[i])
  } else if (instruct_list[i] == "do") {
    do <- TRUE
    values[i] <- 0
  } else {
    values[i] <- 0
  }
    
}

sum(values)
