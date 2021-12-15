library(tidyverse)
library(tidygraph)

input <- read_lines("2021/15_12/input.txt")
input_length <- unique(str_length(input))
increase <- 5L

df_grid_tidy <- tibble(txt = input) |>
  separate(
    col = txt, into = str_glue("col_{seq_len(input_length)}"),
    sep = seq_len(input_length)) |> 
  mutate(row = row_number()) |>
  pivot_longer(cols = starts_with("col"), names_to = "col") |>
  mutate(
    col = parse_integer(str_extract(col, "\\d+")),
    value = parse_integer(value))

erg_vec <- vector(mode = "list", length = increase * increase)
ind <- 1L

increase_grid <- function(df_grid_tidy, i, j, input_length) {
  mutate(
    df_grid_tidy,
    row = row + (i - 1L) * input_length,
    col = col + (j - 1L) * input_length,
    value = (value + i - 1L + j - 1L) %% 9L,
    value = if_else(value == 0L, 9L, value),
    id = str_glue("{row}_{col}"))
}

for (i in seq_len(increase)) {
  for (j in seq_len(increase)) {
    erg_vec[[ind]] <- increase_grid(df_grid_tidy, i, j, input_length)
    ind <- ind + 1L
  }
}

df_grid_tidy_big <- bind_rows(erg_vec)

neigh_vec <- vector(mode = "list", length = nrow(df_grid_tidy_big))

get_neighborhood <- function(df_grid_tidy_big, row_nr, col_nr) {
  df_self <- tibble(row = row_nr, col = col_nr, id = str_glue("{row}_{col}"))
  
  row_range <- unique(
    pmin(pmax(c(row_nr - 1L, row_nr, row_nr + 1L), 1L), max(df_grid_tidy_big$row)))
  col_range <- unique(
    pmin(pmax(c(col_nr - 1L, col_nr, col_nr + 1L), 1L), max(df_grid_tidy_big$col)))
  
  df_grid_tidy_big |>
    filter(
      (row == row_nr & col %in% col_range) | (col == col_nr & row %in% row_range)) |>
    anti_join(df_self, by = c("row", "col")) |>
    rename(to = id) |>
    mutate(from = df_self$id)
}

for (i in seq_along(neigh_vec)) {
  neigh_vec[[i]] <- get_neighborhood(
    df_grid_tidy_big, df_grid_tidy_big$row[[i]], df_grid_tidy_big$col[[i]])
}

df_grid_tidy_neigh <- df_grid_tidy_big |>
  mutate(
    neigh = map2(row, col, ~ get_neighborhood(df_grid_tidy_big, .x, .y)),
    edges = map2(id, neigh, ~ tibble(from = .x, to = .y$id, value = .y$value)))

df_nodes <- df_grid_tidy_neigh |>
  select(id, where(negate(is_list)))

df_edges <- df_grid_tidy_neigh |>
  select(edges) |>
  unnest(edges) |>
  distinct()

cave_graph <- tbl_graph(nodes = df_nodes, edges = df_edges)

shortest_path <- convert(
  cave_graph, to_shortest_path, from = 1, to = nrow(df_nodes), weights = value)

shortest_path |> 
  as_tibble() |>
  filter(id != "1_1") |>
  summarise(res = sum(value))
