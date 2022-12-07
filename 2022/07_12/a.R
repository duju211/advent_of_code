library(tidyverse)
library(tidygraph)

df_commands <- tibble(input = read_lines("2022/07_12/")) |>
  mutate(
    dir = str_extract(str_extract(input, "\\$ cd .$"), ".$"),
    level = cumsum(str_detect(input, "\\$ cd ([a-z]|/)"))
    - cumsum(str_detect(input, "\\$ cd \\.\\.")),
    size = parse_number(input)) |>
  fill(dir)

df_paths <- df_commands |>
  filter(str_detect(input, "^\\$", negate = TRUE)) |>
  mutate(
    is_file = !is.na(size), is_dir = str_detect(input, "^dir")) |>
  bind_rows(
    tibble(
      input = "dir /", dir = NA_character_, level = 0, size = NA_real_,
      is_file = FALSE, is_dir = TRUE))

df_dirs <- df_paths |>
  filter(is_dir) |>
  mutate(name = str_remove(input, "dir\\s"))

df_files <- df_paths |>
  filter(is_file) |>
  mutate(name = str_remove(input, "\\d+\\s"))

df_edges <- bind_rows(df_dirs, df_files) |>
  filter(!is.na(dir)) |>
  select(from = dir, to = name)

df_nodes <- bind_rows(
  list(dirs = df_dirs, files =  df_files), .id = "dir_file") |>
  replace_na(list(size = 0)) |>
  select(name, storage_size = size, dir_file)

res_graph <- tbl_graph(nodes = df_nodes, edges = df_edges)

#debug(sum_size)

df_res_graph <- res_graph |>
  mutate(
    total_size = map_dfs_back_dbl(
      root = node_is_root(), .f = function(node, path, ...) {
        if (nrow(path) == 0) {
          .N()$storage_size[node]
        } else {
          sum(unlist(path$result))
        }
      }))

df_res_graph |>
  as_tibble() |>
  filter(dir_file == "dirs", total_size <= 100000) |>
  pull(total_size) |>
  sum()

         