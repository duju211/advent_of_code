library(tidyverse)

input <- read_lines("2022/02_12/input.txt")

df_reward <- tibble(
  abc = c("A", "B", "C"),
  reward = c(1, 2, 3))

df_result <- tibble(
  abc = c("A", "B", "C"),
  win = c("B", "C", "A"),
  draw = c("A", "B", "C"),
  lose = c("C", "A", "B"))

df_games_raw <- tibble(input = input) |>
  separate(input, c("elv", "me"))

df_games <- df_games_raw |>
  left_join(df_result, by = c("elv" = "abc")) |>
  mutate(
    move = case_when(
      me == "X" ~ lose,
      me == "Y" ~ draw,
      me == "Z" ~ win)) |>
  left_join(df_reward, by = c("move" = "abc")) |>
  mutate(
    result = case_when(
      move == win ~ 6L,
      move == draw ~ 3L,
      move == lose ~ 0L,
      TRUE ~ NA_integer_),
    points = reward + result)

df_games |>
  pull(points) |>
  sum()