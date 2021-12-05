library(tidyverse)

df_input_raw <- tibble(txt = read_lines("2021/04_12/input.txt"))

ind_numbers <- min(which(df_input_raw$txt == ""))
numbers <- df_input_raw[1:(ind_numbers - 1), ] |>
  pull() |>
  str_split(",") |>
  first() |>
  as.integer()

df_boards <- df_input_raw |>
  slice(-c(1:ind_numbers)) |>
  mutate(index = cumsum(txt == "")) |>
  filter(txt != "") |>
  separate(
    col = txt, into = c("col1", "col2", "col3", "col4", "col5"),
    sep = "\\s+") |>
  mutate(across(where(is.character), as.integer)) |>
  nest(board = -index)

board_win_when <- function(df_board) {
  for (i in seq_along(numbers)) {
    numbers_seq <- numbers[1:i]
    for (j in seq_len(nrow(df_board))) {
      if (all(df_board[j, ] %in% numbers_seq)) {
        return(i)
      }
      if (all(pull(df_board[, j]) %in% numbers_seq)) {
        return(i)
      }
    }
  }
}

df_boards_results <- df_boards |>
  mutate(win_when = map_int(board, board_win_when))

df_winning_board <- df_boards_results |>
  filter(win_when == min(win_when))

df_losing_board <- df_boards_results |>
  filter(win_when == max(win_when))

#solution 1
df_winning_board |>
  unnest(board) |>
  pivot_longer(cols = starts_with("col")) |>
  filter(!(value %in% numbers[1:unique(df_winning_board$win_when)])) |>
  summarise(result = sum(value) * numbers[unique(df_winning_board$win_when)])

#solution 2
df_losing_board |>
  unnest(board) |>
  pivot_longer(cols = starts_with("col")) |>
  filter(!(value %in% numbers[1:unique(df_losing_board$win_when)])) |>
  summarise(result = sum(value) * numbers[unique(df_losing_board$win_when)])
