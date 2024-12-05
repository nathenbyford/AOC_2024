library(tidyverse)

# Part 1

data <- read_delim("day_1.txt", delim = "   ", col_names = FALSE)

x1 <- data$X1[order(data$X1)]

x2 <- data$X2[order(data$X2)]

d <- abs(x1 - x2)

sum(d)


# Part 2

sim_score <- numeric()

for (i in 1:length(x1)) {
    sim_score[i] <- x1[i] * sum(x2 == x1[i])
}

sum(sim_score)
