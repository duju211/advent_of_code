library(tidyverse)

input <- read_lines("2022/05_12/input")
divide <- which(input == "") - 1

df_crates_raw <- tibble(input = input[1:divide]) |>
  mutate(ind = divide - row_number())

df_moves_raw <- tibble(
  input = input[(which(input == "") + 1):length(input)])

df_moves <- df_moves_raw |>
  mutate(
    split = str_split(input, "\\s"),
    anz = map_int(split, ~ parse_integer(.x[[2]])),
    from = map_int(split, ~ parse_integer(.x[[4]])),
    to = map_int(split, ~ parse_integer(.x[[6]])))

df_base <- slice(df_crates_raw, nrow(df_crates_raw)) |>
  mutate(
    split = str_split(input, ""),
    stack_ind = map(split, str_which, "\\d"))

df_crates <- df_crates_raw |>
  anti_join(df_base, by = "ind") |>
  mutate(
    stack_ind = df_base$stack_ind,
    crate = map2(input, stack_ind, ~ str_sub(.x, start = .y, end = .y))) |>
  unnest(crate) |>
  group_by(ind) |>
  mutate(
    col_nr = row_number(),
    crate = if_else(str_trim(crate) == "", NA_character_, crate)) |>
  ungroup() |>
  select(crate, ind, col_nr) |>
  filter(!is.na(crate))

move_crates <- function(df_crates, from, to, anz) {
  df_from <- df_crates |>
    filter(col_nr == from, ind > max(ind[col_nr == from]) - anz)
  
  new_start_ind <- max(0, max(df_crates$ind[df_crates$col_nr == to]))
  df_from_pro <- df_from |>
    arrange(ind) |>
    mutate(
      col_nr = to,
      ind = new_start_ind + seq(length.out = anz))
  
  df_crates |>
    anti_join(df_from, by = c("col_nr", "ind")) |>
    bind_rows(df_from_pro)
}

df_crates_tmp <- df_crates
for (i in seq_along(res))  {
  df_crates_tmp <- move_crates(
    df_crates_tmp, from = df_moves$from[[i]], to = df_moves$to[[i]],
    anz = df_moves$anz[[i]])
}

df_crates_tmp |>
  group_by(col_nr) |>
  summarise(top = crate[ind == max(ind)]) |>
  pull(top) |>
  str_flatten()