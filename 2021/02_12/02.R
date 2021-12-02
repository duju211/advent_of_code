library(tidyverse)

tibble(
  txt = read_lines(here::here("2021", "02_12", "input.txt"))) |>
  separate(txt, into = c("direction", "units"), sep = "\\s") |>
  mutate(
    units = as.integer(units),
    units = if_else(direction == "up", units * (-1L), units),
    orientation = if_else(direction == "forward", "horizontal", "depth")) |>
  group_by(orientation) |>
  summarise(units = sum(units)) |>
  summarise(erg = prod(units)) |>
  pull()

tibble(
  txt = read_lines(here::here("2021", "02_12", "input.txt"))) |>
  separate(txt, into = c("direction", "units"), sep = "\\s") |>
  mutate(
    units = as.integer(units),
    orientation = if_else(direction == "forward", "horizontal", "depth"),
    aim = cumsum(case_when(
      direction == "down" ~ units,
      direction == "up" ~ units * (-1L),
      TRUE ~ 0L)),
    horizontal_pos = if_else(
      direction == "forward", units, 0L),
    depth_pos = if_else(
      direction == "forward", aim * units, 0L),
    across(c(horizontal_pos, depth_pos), cumsum)) |>
  summarise(erg = last(horizontal_pos) * last(depth_pos)) |>
  pull()
