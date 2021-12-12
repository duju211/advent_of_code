library(tidyverse)

input <- read_lines("2021/10_12/input.txt")

df_brackets <- tibble(
  opening = c("(", "[", "{", "<"), closing = c(")", "]", "}", ">"),
  error_points = c(3L, 57L, 1197L, 25137L))

df_brackets_long <- df_brackets |>
  select(-error_points) |>
  pivot_longer(cols = everything(), names_to = "type")

df_row_brackets <- tibble(txt = input) |>
  mutate(
    row_nr = row_number(), bracket = str_split(txt, "")) |>
  unnest(bracket) |>
  left_join(df_brackets_long, by = c("bracket" = "value")) |>
  group_by(row_nr) |>
  mutate(cum_closing = cumsum(type == "closing"), bracket_nr = row_number()) |>
  nest() |>
  ungroup()

first_error <- function(test) {
  for (i in seq_len(max(test$cum_closing))) {
    closing_bracket <- test[min(which(test$cum_closing == i)), ]
    
    opening_bracket <- test |>
      filter(bracket_nr < closing_bracket$bracket_nr) |>
      filter(type == "opening", bracket_nr == max(bracket_nr))
    
    if (opening_bracket$bracket == df_brackets$opening[df_brackets$closing == closing_bracket$bracket]) {
      test <- test |>
        anti_join(opening_bracket, by = "bracket_nr") |>
        anti_join(closing_bracket, by = "bracket_nr")
    } else {
      return(closing_bracket)
    }
  }
}

df_result <- df_row_brackets |>
  mutate(first_error = map(data, first_error))

df_result |>
  mutate(
    error_bracket = map_chr(map_if(
      .x = first_error, .p = ~ !is.null(.x), .f = ~ .x$bracket,
      .else = ~ NA_character_), 1)) |>
  filter(!is.na(error_bracket)) |>
  left_join(df_brackets, by = c("error_bracket" = "closing")) |>
  summarise(erg = sum(error_points))
