library(visNetwork)
library(tidygraph)
library(tidyverse)
library(igraph)

input <- read_lines("2021/12_12/input_ex1.txt")

df_edges <- tibble(txt = input) |>
  separate(col = txt, into = c("from", "to"), sep = "-")

df_nodes <- df_edges |>
  pivot_longer(cols = everything()) |>
  distinct(name = value) |>
  mutate(uppercase = str_detect(name, "^[A-Z]"))

df_edges_pro <- df_edges |>
  filter(str_detect(from, "^[A-Z]") | str_detect(to, "^[A-Z]")) |>
  mutate(new_from = to, to = from) |>
  select(from = new_from, to) |>
  bind_rows(df_edges)

df_edges_ind <- df_edges_pro |>
  transmute(
    from = map_int(from, ~ which(df_nodes$name == .x)),
    to = map_int(to, ~ which(df_nodes$name == .x)))

graph <- tbl_graph(nodes = df_nodes, edges = df_edges_ind, directed = TRUE)

graph