library(tidyverse)

input <- read_lines("2021/13_12/input.txt")

df_coord <- tibble(txt = input) |>
  filter(str_detect(txt, "^\\d")) |>
  separate(txt, c("col", "row")) |>
  mutate(across(everything(), ~ parse_integer(.x) + 1L), id = row_number()) |>
  pivot_longer(cols = -id, names_to = "row_col")

df_fold_instructions <- tibble(
  fold_instructions = str_subset(input, "^fold"),
  fold_direction = str_extract(fold_instructions, "x|y"),
  fold_units = parse_integer(str_extract(fold_instructions, "\\d+"))) |>
  mutate(
    fold_units = fold_units + 1L,
    fold_direction = if_else(fold_direction == "y", "row", "col"))

coord_fold <- function(df_coord, dec_var, fold_line) {
  df_coord_fold <- df_coord |>
    group_by(id) |>
    filter(value[row_col == dec_var] > fold_line) |>
    ungroup() |> 
    mutate(
      value = if_else(
        row_col == dec_var, max(value[row_col == dec_var]) + 1L - value, value))
  
  df_coord |>
    group_by(id) |>
    filter(value[row_col == dec_var] < fold_line) |>
    ungroup() |> 
    bind_rows(df_coord_fold) |>
    pivot_wider(names_from = row_col, values_from = value) |>
    distinct(row, col, .keep_all = TRUE) |>
    arrange(row, col) %>%
    pivot_longer(cols = -id, names_to = "row_col")
}

df_new_fold <- coord_fold(
  df_coord,
  dec_var = df_fold_instructions[1, ]$fold_direction,
  fold_line = df_fold_instructions[1, ]$fold_units)

nrow(distinct(df_new_fold, id))
ind <- 2L

for (i in ind:nrow(df_fold_instructions)) {
  df_new_fold <- coord_fold(
    df_new_fold,
    dec_var = df_fold_instructions$fold_direction[[i]],
    fold_line = df_fold_instructions$fold_units[[i]])
}

df_erg <- df_new_fold %>%
  pivot_wider(names_from = row_col, values_from = value) %>%
  arrange(col) %>%
  mutate(id = "x") %>% 
  pivot_wider(names_from = col, values_from = id) %>%
  arrange(row)
