library(tidyverse)

tibble(
  number = as.integer(read_lines(here::here("2021", "01_12", "input.txt")))) |>
  tally(number > lag(number)) |>
  pull()

tibble(
  number = as.integer(read_lines(here::here("2021", "01_12", "input.txt")))) |>
  mutate(
    lag_1 = lag(number), lag_2 = lag(lag_1),
    sum = number + lag_1 + lag_2, sum_lag = lag(sum)) |>
  tally(sum > sum_lag) |>
  pull()
