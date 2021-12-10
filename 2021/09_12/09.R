library(tidyverse)

input <- read_lines("2021/09_12/input.txt")
input_length <- unique(str_length(input))

df_grid <- tibble(txt = input) |>
  separate(
    txt, into = str_glue("col_{seq_len(input_length)}"),
    sep = seq_len(input_length - 1)) |>
  mutate(across(everything(), as.integer))

get_neighborhood <- function(df_grid, i, j, filter_greater = FALSE) {
  ln <- c(j, max(i - 1L, 1L))
  rn <- c(j, min(i + 1L, ncol(df_grid)))
  un <- c(max(j - 1L, 1L), i)
  bn <- c(min(j + 1L, nrow(df_grid)), i)
  
  tibble(coord = list(ln, rn, un, bn)) %>%
    transmute(row = map_int(coord, 1), col = map_int(coord, 2)) %>%
    filter(!(row == j & col == i)) %>% 
    mutate(value = map2_int(row, col, ~ df_grid[[.x, .y]])) %>%
    {if(filter_greater) filter(., value > df_grid[[j, i]], value != 9)  else .}
}

low_points <- vector(mode = "list")
ind <- 1L

for (i in seq_len(ncol(df_grid))) {
  for (j in seq_len(nrow(df_grid))) {
    df_neigh <- get_neighborhood(df_grid, i, j)
    
    if (all(df_grid[[j, i]] < df_neigh$value)) {
      low_points[[ind]] <- df_neigh |>
        mutate(center = FALSE) |>
        add_row(row = j, col = i, value = df_grid[[j, i]], center = TRUE)
      ind <- ind + 1
    }
  }
}

sum(map_int(low_points, ~ .x$value[.x$center]) + 1)

fill_sink <- function(low_point) {
  df_sink <- filter(low_point, center)
  new_neighbors <- df_sink
  
  while(nrow(new_neighbors) != 0) {
    new_neighbors <- new_neighbors %>%
      transmute(
        neigh = map2(row, col, ~ get_neighborhood(df_grid, .y, .x, filter_greater = TRUE))) %>%
      unnest(neigh) %>%
      anti_join(df_sink, by = c("row", "col"))
    df_sink <- df_sink %>% 
      bind_rows(new_neighbors) %>%
      distinct()
  }
  return(df_sink)
}

tibble(low_point = low_points) %>%
  mutate(
    sink = map(low_point, fill_sink), sink_size = map_int(sink, nrow)) %>%
  top_n(n = 3, wt = sink_size) %>%
  summarise(result = prod(sink_size))
