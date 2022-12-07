library(tidyverse)
library(tidygraph)

df_commands <- tibble(input = read_lines("2022/07_12/input")) |>
  mutate(command_nr = row_number())

df_cd <- df_commands |>
  filter(str_detect(input, "\\$ cd ")) |>
  transmute(command_nr, dir = str_remove(input, "\\$ cd "))

init_stack <- character()
erg_vec <- vector(mode = "list", length = nrow(df_cd))
for (i in seq_len(nrow(df_cd))) {
  if (df_cd$dir[[i]] != "..") {
    init_stack <- c(init_stack, df_cd$dir[[i]])
  } else {
    init_stack <- init_stack[-length(init_stack)]
  }
  erg_vec[[i]] <- init_stack
}

df_stack <- df_cd |>
  transmute(command_nr, stack = erg_vec)

df_paths <- df_commands |>
  left_join(df_stack, by = "command_nr") |>
  fill(stack) |>
  filter(str_detect(input, "\\$ ", negate = TRUE)) |>
  mutate(parent = map_chr(stack, str_flatten, "_"))

df_dirs <- df_paths |>
  filter(str_detect(input, "^dir")) |>
  mutate(dir_name = str_remove(input, "^dir ")) |>
  transmute(name = str_glue("{parent}_{dir_name}"), parent) |>
  bind_rows(tibble(name = "/", parent = NA_character_))

df_files <- df_paths |>
  filter(str_detect(input, "^\\d+")) |>
  mutate(file_name = str_remove(input, "^\\d+\\s")) |>
  transmute(
    name = str_glue("{parent}_{file_name}"), parent, size = parse_number(input)) 

df_edges <- bind_rows(df_dirs, df_files) |>
  filter(!is.na(parent)) |>
  select(from = parent, to = name)

df_nodes <- bind_rows(
  list(dirs = df_dirs, files =  df_files), .id = "dir_file") |>
  replace_na(list(size = 0)) |>
  select(name, storage_size = size, dir_file) |>
  distinct()

res_graph <- tbl_graph(nodes = df_nodes, edges = df_edges)

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

df_res_dirs <- df_res_graph |>
  as_tibble() |>
  filter(dir_file == "dirs")

unused_space <- 70000000 - max(df_res_dirs$total_size)
at_least_to_delete <- 30000000 - unused_space

df_res_dirs |>
  filter(total_size >= at_least_to_delete) |>
  pull(total_size) |>
  min()
