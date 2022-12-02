library(tidyverse)

input <- read_lines("2022/02_12/input.txt")

df_trans <- tibble(
  abc = c("A", "B", "C"),
  xyz = c("X", "Y", "Z"))

df_reward <- tibble(
  abc = c("A", "B", "C"),
  reward = c(1, 2, 3))

df_games <- tibble(input = input) |>
  separate(input, c("elv", "me")) |>
  left_join(df_trans, by = c("me" = "xyz")) |>
  left_join(df_reward, by = c("abc")) |>
  mutate(
    result = case_when(
      elv == abc ~ 3,
      elv == "A" & abc == "B" ~ 6,
      elv == "B" & abc == "C" ~ 6,
      elv == "C" & abc == "A" ~ 6,
      TRUE ~ 0),
    points = reward + result)

df_games |>
  pull(points) |>
  sum()
