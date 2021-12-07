library(tidyverse)

input <- read_lines("2021/07_12/input.txt")

positions <- as.integer(str_split(input, ",")[[1]])

tibble(
  position = min(positions):max(positions)) |>
  mutate(diff = map(position, ~ abs(positions - .x))) |>
  unnest(diff) |>
  mutate(diff_sum = map_int(diff, ~ sum(seq_len(.x)))) |>
  group_by(position) |>
  summarise(erg = sum(diff_sum)) |>
  arrange(erg)
