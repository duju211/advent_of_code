library(tidyverse)

input <- read_lines("2022/02_12/input_ex.txt")

df_trans <- tibble(
  abc = c("A", "B", "C"),
  xyz = c("X", "Y", "Z"))

df_reward <- tibble(
  abc = c("A", "B", "C"),
  reward = c(1, 2, 3))

df_win <- tibble(
  abc = c("A", "B", "C"),
  win = c("B", "C", "A"))

df_draw <- tibble(
  abc = c("A", "B", "C"),
  draw = c("A", "B", "C"))

df_lose <- tibble(
  abc = c("A", "B", "C"),
  lose = c("C", "A", "B"))

df_games_raw <- tibble(input = input) |>
  separate(input, c("elv", "me"))

df_games <- df_games_raw |>
  left_join(df_win, by = c("elv" = "abc")) |>
  left_join(df_draw, by = c("elv" = "abc")) |>
  left_join(df_lose, by = c("elv" = "abc")) |>
  mutate(
    move = case_when(
      me == "X" ~ lose,
      me == "Y" ~ draw,
      me == "Z" ~ win)) |>
  left_join(df_reward, by = c("move" = "abc")) |>
  mutate(
    result = case_when(
      elv == move ~ 3,
      elv == "A" & move == "B" ~ 6,
      elv == "B" & move == "C" ~ 6,
      elv == "C" & move == "A" ~ 6,
      TRUE ~ 0),
    points = reward + result)

df_games |>
  pull(points) |>
  sum()
