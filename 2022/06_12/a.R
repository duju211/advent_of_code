library(tidyverse)

input <- read_lines("2022/06_12/input")

df_lags <- tibble(lag_0 = str_split(input, "")[[1]]) |>
  mutate(
    lag_1 = lag(lag_0), lag_2 = lag(lag_0, 2), lag_3 = lag(lag_0, 3)) |>
  filter(!is.na(lag_3)) |>
  mutate(position_first = row_number(), position_last = position_first + 3)

df_lags_long <- df_lags |>
  pivot_longer(cols = contains("lag"))

df_lags_long |>
  group_by(position_last) |>
  summarise(complete = length(value) == length(unique(value))) |>
  filter(complete) |>
  pluck("position_last", 1)
