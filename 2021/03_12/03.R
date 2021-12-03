library(tidyverse)

input <- read_lines("2021/03_12/input.txt")
# input <- c(
#   "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100",
#   "10000", "11001", "00010", "01010")

input_length <- unique(str_length(input))
col_names <- str_glue("bit_{1:input_length}")

df_bits <- tibble(txt = input) |>
  separate(
    txt, into = col_names, sep = 1:input_length) |>
  mutate(across(everything(), as.integer), row_nr = row_number()) |>
  pivot_longer(
    cols = c(starts_with("bit")), names_to = "bit_nr", values_to = "value") |>
  mutate(bit_nr = as.integer(str_extract(bit_nr, "\\d+")) - 1)

df_bits |>
  group_by(bit_nr) |>
  summarise(
    mode = median(value), mode_inv = unique(value)[unique(value) != mode]) |>
  mutate(across(c(mode, mode_inv), ~ .x * 2^(input_length - 1- bit_nr))) |>
  summarise(erg = sum(mode) * sum(mode_inv)) |>
  pull()
