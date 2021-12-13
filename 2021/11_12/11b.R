library(tidyverse)

input <- read_lines("2021/11_12/input.txt")
input_length <- unique(str_length(input))

df_grid <- tibble(txt = input) |>
  separate(
    txt, into = str_glue("col_{seq_len(input_length)}"),
    sep = seq_len(input_length - 1)) |>
  mutate(across(everything(), as.integer))

df_grid_tidy <- df_grid |>
  mutate(row = row_number()) |>
  pivot_longer(cols = -row, names_to = "col") |>
  mutate(col = parse_integer(str_extract(col, "\\d+")))

get_neighborhood <- function(df_grid_tidy, row_nr, col_nr) {
  row_range <- unique(
    pmin(pmax(c(row_nr - 1L, row_nr, row_nr + 1L), 1L), nrow(df_grid)))
  col_range <- unique(
    pmin(pmax(c(col_nr - 1L, col_nr, col_nr + 1L), 1L), ncol(df_grid)))
  
  df_grid_tidy |>
    filter(row %in% row_range & col %in% col_range) |>
    anti_join(tibble(row = row_nr, col = col_nr), by = c("row", "col"))
}

sim_flashes <- function(df_grid_tidy) {
  df_grid_tidy_now <- df_grid_tidy |>
    mutate(value = value + 1L)
  
  df_flash <- df_grid_tidy_now |>
    filter(value == 10L) |>
    mutate(value = 0L)
  df_flash_all <- df_flash
  
  while (nrow(df_flash) != 0) {
    df_neigh <- df_flash |>
      transmute(
        neighborhood = map2(
          row, col, ~ get_neighborhood(df_grid_tidy_now, .x, .y))) |>
      unnest(neighborhood) |>
      anti_join(df_flash_all, by = c("row", "col")) |>
      group_by(row, col) |>
      summarise(value = unique(value) + n(), .groups = "drop")
    
    df_grid_tidy_now <- df_grid_tidy_now |>
      anti_join(df_neigh, by = c("row", "col")) |>
      anti_join(df_flash, by = c("row", "col")) |>
      bind_rows(bind_rows(df_flash, df_neigh))
    
    df_flash <- df_grid_tidy_now |>
      filter(value >= 10L) |>
      mutate(value = 0L)
    
    df_flash_all <- df_flash_all |>
      bind_rows(df_flash_all, df_flash) |>
      distinct()
  }
  df_grid_tidy_now
}

df_grid_today <- sim_flashes(df_grid_tidy)
ind <- 1L
while(!all(df_grid_today$value == 0)) {
  df_grid_today <- sim_flashes(df_grid_today)
  ind <- ind + 1L
}
ind
