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

df_inner_neigh <- df_inner |> 
  unnest(neigh) |>
  mutate(
    area = case_when(
      col == tree_col & row < tree_row ~ "u",
      col == tree_col & row > tree_row ~ "d",
      row == tree_row & col < tree_col ~ "l",
      row == tree_row & col > tree_col ~ "r"))

df_scenic <- df_inner_neigh |>
  nest(neigh = -c(tree_number, tree_row, tree_col, area)) |>
  mutate(
    row_col = if_else(area %in% c("l", "r"), "col", "row"),
    neigh = map2(neigh, row_col, ~ rename(.x, index = .y)),
    decr = area %in% c("u", "l"),
    neigh = map_if(
      .x = neigh, .p = decr,
      .f = ~ arrange(.x, desc(index)), .else = ~ arrange(.x, index)),
    scenic_score = map2_dbl(
      neigh, tree_number,
      ~ min(sum(cumall(.x$number < .y)) + 1, length(.x$number))))

df_scenic_total <- df_scenic |>
  group_by(tree_number, tree_row, tree_col) |>
  summarise(total_scenic_score = prod(scenic_score), .groups = "drop") |>
  arrange(desc(total_scenic_score))

pluck(df_scenic_total, "total_scenic_score", 1)
