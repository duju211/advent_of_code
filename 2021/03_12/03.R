library(tidyverse)

input <- read_lines("2021/03_12/input.txt")

input_length <- unique(str_length(input)) - 1

df_bits <- tibble(txt = input) |>
  separate(
    txt, into = c("bit_1", "bit_2", "bit_3", "bit_4", "bit_5"),
    sep = c(1, 2, 3, 4)) |>
  mutate(across(everything(), as.integer), row_nr = row_number()) |>
  pivot_longer(
    cols = c(starts_with("bit")), names_to = "bit_nr", values_to = "value") |>
  mutate(bit_nr = as.integer(str_extract(bit_nr, "\\d+")) - 1)

df_bits |>
  group_by(bit_nr) |>
  summarise(main_value = median(value)) |>
  mutate(erg_value = main_value * 2^(input_length - bit_nr)) |>
  summarise(erg = sum(erg_value)) |>
  pull()
