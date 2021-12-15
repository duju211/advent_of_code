library(tidyverse)
library(tidygraph)

input <- read_lines("2021/15_12/input.txt")
input_length <- unique(str_length(input))

df_grid_tidy <- tibble(txt = input) |>
  separate(
    col = txt, into = str_glue("col_{seq_len(input_length)}"),
    sep = seq_len(input_length)) |> 
  mutate(row = row_number()) |>
  pivot_longer(cols = starts_with("col"), names_to = "col") |>
  mutate(
    col = parse_integer(str_extract(col, "\\d+")),
    value = parse_integer(value),
    id = str_glue("{row}_{col}"))

get_neighborhood <- function(df_grid_tidy, row_nr, col_nr) {
  row_range <- unique(
    pmin(pmax(c(row_nr - 1L, row_nr, row_nr + 1L), 1L), max(df_grid_tidy$row)))
  col_range <- unique(
    pmin(pmax(c(col_nr - 1L, col_nr, col_nr + 1L), 1L), max(df_grid_tidy$col)))
  
  df_grid_tidy |>
    filter(
      (row == row_nr & col %in% col_range) | (col == col_nr & row %in% row_range)) |>
    anti_join(tibble(row = row_nr, col = col_nr), by = c("row", "col"))
}


df_grid_tidy_neigh <- df_grid_tidy |>
  mutate(
    neigh = map2(row, col, ~ get_neighborhood(df_grid_tidy, .x, .y)),
    edges = map2(id, neigh, ~ tibble(from = .x, to = .y$id, value = .y$value)))

df_edges <- df_grid_tidy_neigh |>
  select(edges) |>
  unnest(edges) |>
  distinct()

df_nodes <- df_grid_tidy_neigh |>
  select(id, where(negate(is_list)))

cave_graph <- tbl_graph(nodes = df_nodes, edges = df_edges)

shortest_path <- convert(
  cave_graph, to_shortest_path, from = 1, to = nrow(df_nodes), weights = value)

shortest_path |> 
  as_tibble() |>
  filter(id != "1_1") |>
  summarise(res = sum(value))
