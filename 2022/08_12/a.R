library(tidyverse)

df_input <- tibble(input = read_lines("2022/08_12/input"))

df_grid <- df_input |>
  mutate(number = str_split(input, ""), row = row_number()) |>
  unnest(cols = c(number)) |>
  mutate(number = parse_integer(number)) |>
  group_by(row) |>
  mutate(col = row_number()) |>
  ungroup()

df_neigh <- df_grid |>
  mutate(
    neigh = map2(
      row, col,
      ~ df_grid |>
        filter(
          col == .y | row == .x) |>
        filter(!(row == .x & col == .y)) |>
        select(-input))) |>
  rename(tree_number = number, tree_row = row, tree_col = col)

df_outer <- df_neigh |>
  filter(
    (tree_row == min(tree_row) | tree_row == max(tree_row))
    | (tree_col == min(tree_col) | tree_col == max(tree_col)))

df_inner <- df_neigh |>
  anti_join(df_outer, by = c("tree_row", "tree_col"))

df_inner_visible <- df_inner |> 
  unnest(neigh) |>
  mutate(
    area = case_when(
      col == tree_col & row < tree_row ~ "d",
      col == tree_col & row > tree_row ~ "u",
      row == tree_row & col < tree_col ~ "l",
      row == tree_row & col > tree_col ~ "r")) |>
  group_by(tree_number, tree_row, tree_col, area) |>
  summarise(visible = all(number < tree_number), .groups = "drop_last") |>
  summarise(visible = any(visible), .groups = "drop") |>
  filter(visible)

nrow(df_outer) + nrow(df_inner_visible)
