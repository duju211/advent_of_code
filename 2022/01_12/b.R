library(tidyverse)

input <- read_lines("2022/01_12/input")

df_elves <- tibble(
  calories = input) |>
  mutate(
    elv_number = cumsum(calories == ""), calories = parse_number(calories)) |>
  filter(!is.na(calories))

df_elves |>
  group_by(elv_number) |>
  summarise(sum_calories = sum(calories), .groups = "drop") |>
  top_n(n = 3, wt = sum_calories) |>
  pull(sum_calories) |>
  sum()
