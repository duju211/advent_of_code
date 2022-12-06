library(tidyverse)

input <- str_split(read_lines("2022/06_12/input"), "")[[1]]

df_lags <- 1:14 |>
  map_df(
    ~ tibble(
      lag_0 = input, lag_n = lag(input, .x), n = .x) |>
      mutate(position_to = row_number() - 1, position_from = position_to - 14)) |>
  group_by(position_from, position_to) |>
  filter(all(!is.na(lag_n)))

df_lags |>
  summarise(
    complete = length(lag_n) == length(unique(lag_n)), .groups = "drop") |>
  arrange(position_to) |>
  filter(complete) |>
  pluck("position_to", 1)
