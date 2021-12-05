library(tidyverse)

input <- read_lines("2021/05_12/input.txt")

df_vents_raw <- tibble(txt = input) |>
  mutate(
    txt_split = str_split(txt, " -> "),
    start_coord = map_chr(txt_split, 1), end_coord = map_chr(txt_split, 2),
    across(c(start_coord, end_coord), str_split, pattern = ","),
    start_coord_x = map_chr(start_coord, 1),
    start_coord_y = map_chr(start_coord, 2),
    end_coord_x = map_chr(end_coord, 1),
    end_coord_y = map_chr(end_coord, 2),
    across(
      c(start_coord_x, start_coord_y, end_coord_x, end_coord_y), as.integer),
    across(
      c(start_coord_x, start_coord_y, end_coord_x, end_coord_y), ~ .x + 1)) |>
  select(where(negate(is_list)))

df_vents <- df_vents_raw |>
  mutate(
    x = map2(start_coord_x, end_coord_x, ~ .x:.y),
    y = map2(start_coord_y, end_coord_y, ~ .x:.y)) |>
  filter(map_int(x, ~ length(.x) == 1) | map_int(y, ~ length(.x) == 1))

df_dim <- df_vents |>
  mutate(max_x = map_int(x, max), max_y = map_int(y, max)) |>
  summarise(max_x = max(max_x), max_y = max(max_y))

grid_mat <- matrix(0L, nrow = df_dim$max_y, ncol = df_dim$max_x)
df_grid <- as_tibble(grid_mat)

change_grid <- function(df_grid, x, y) {
  df_grid[y, x] <- df_grid[y, x] + 1
  df_grid
}

for (i in seq_len(nrow(df_vents))) {
  df_grid <- change_grid(df_grid, df_vents$x[[i]], df_vents$y[[i]])
}

sum(map_int(df_grid, ~ sum(.x > 1)))
