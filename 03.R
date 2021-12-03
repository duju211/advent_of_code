library(tidyverse)

example_input <- c(
  "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100",
  "10000", "11001", "00010", "01010")


df_bits <- tibble(txt = example_input) |>
  separate(
    txt, into = c("bit_1", "bit_2", "bit_3", "bit_4", "bit_5"),
    sep = c(1, 2, 3, 4)) |>
  mutate(across(everything(), as.integer), row_nr = row_number()) |>
  pivot_longer(
    cols = c(starts_with("bit")), names_to = "bit_nr", values_to = "value")

df_bits |>
  group_by(bit_nr) |>
