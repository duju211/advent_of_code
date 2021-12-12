library(tidyverse)

input <- read_lines("2021/10_12/input.txt")

df_brackets <- tibble(
  opening = c("(", "[", "{", "<"), closing = c(")", "]", "}", ">"),
  error_points = c(1L, 2L, 3L, 4L))

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
      stop("Incomplete")
    }
  }
  return(test)
}

df_result <- df_row_brackets |>
  mutate(incomplete = map(data, possibly(first_error, NULL)))

df_result_good <- df_result |>
  filter(!map_lgl(incomplete, is.null)) |>
  select(-data)

calc_score <- function(test, df_brackets) {
  test_pro <- test |>
    left_join(df_brackets, by = c("bracket" = "opening")) |>
    arrange(desc(bracket_nr))
  
  score <- 0L
  erg_vec <- vector(mode = "integer", length = nrow(test))
  
  for (i in seq_along(test_pro$error_points)) {
    erg_vec[[i]] <- score * 5 + test_pro$error_points[[i]]
    score <- erg_vec[[i]]
  }
  score
}

df_result_good |>
  mutate(score = map_dbl(incomplete, ~ calc_score(.x, df_brackets))) |>
  summarise(erg = median(score))
