library(tidyverse)

#input <- "16,1,2,0,4,2,7,1,2,14"
input <- read_lines("2021/07_12/input.txt")

positions <- as.integer(str_split(input, ",")[[1]])

fuel_con <- map_int(min(positions):max(positions), ~ sum(abs(positions - .x)))
min_ind <- which.min(fuel_con)
fuel_con[min_ind]
