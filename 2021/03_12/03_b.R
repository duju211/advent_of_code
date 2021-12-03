library(tidyverse)

input <- c(
  "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100",
  "10000", "11001", "00010", "01010")
input <- read_lines("2021/03_12/input.txt")
input_length <- unique(str_length(input))
col_names <- str_glue("bit_{1:input_length}")

df_bits <- tibble(txt = input) |>
  separate(
    txt, into = col_names, sep = 1:input_length) |>
  mutate(across(everything(), as.integer))

df_bits_oxygen <- df_bits
df_bits_co2 <- df_bits

erg_vec_oxygen <- vector(mode = "list", length = input_length)
for (i in seq_len(input_length)) {
  if (sum(df_bits_oxygen[, i]) >= nrow(df_bits_oxygen) / 2)
    most_freq <- 1
  else
    most_freq <- 0
  erg_vec_oxygen[[i]] <- df_bits_oxygen[df_bits_oxygen[, i] == most_freq, ]
  df_bits_oxygen <- df_bits_oxygen[df_bits_oxygen[, i] == most_freq,]
}
ind_sol <- min(which(map_int(erg_vec_oxygen, nrow) == 1))         
erg_vec_oxygen[[ind_sol]] |>
  pivot_longer(cols = everything()) |>
  mutate(name = input_length - as.integer(str_extract(name, "\\d+"))) |>
  summarise(erg = sum(value * 2^name))

erg_vec_co2 <- vector(mode = "list", length = input_length)
for (i in seq_len(input_length)) {
  if (sum(df_bits_co2[, i]) < nrow(df_bits_co2) / 2)
    most_freq <- 1
  else
    most_freq <- 0
  erg_vec_co2[[i]] <- df_bits_co2[df_bits_co2[, i] == most_freq, ]
  df_bits_co2 <- df_bits_co2[df_bits_co2[, i] == most_freq,]
}
ind_sol <- min(which(map_int(erg_vec_co2, nrow) == 1))         
erg_vec_co2[[ind_sol]] |>
  pivot_longer(cols = everything()) |>
  mutate(name = input_length - as.integer(str_extract(name, "\\d+"))) |>
  summarise(erg = sum(value * 2^name))
  
