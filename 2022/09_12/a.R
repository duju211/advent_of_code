library(tidyverse)

df_moves <- tibble(input = read_lines("2022/09_12/input")) |>
  separate(input, into = c("direction", "count")) |>
  mutate(
    count = parse_integer(count), direction = str_to_lower(direction),
    direction = map2(direction, count, rep)) |>
  unnest(direction) |>
  mutate(count = if_else(direction %in% c("l", "d"), -1, 1))

pos_head <- c(0, 0)
pos_tail <- c(0, 0)
erg_vec <- vector(mode = "list", length = nrow(df_moves))

for (i in seq_len(nrow(df_moves))) {
  df_new_row <- tibble(
    mov_nr = i, pos_head_x = pos_head[[1]], pos_head_y = pos_head[[2]],
    pos_tail_x = pos_tail[[1]], pos_tail_y = pos_tail[[2]])
  erg_vec[[i]] <- df_new_row
  if (df_moves$direction[[i]] %in% c("l", "r")) {
    pos_head[[1]] <- pos_head[[1]] + df_moves$count[[i]]
  } else {
    pos_head[[2]] <- pos_head[[2]] + df_moves$count[[i]]
  }
  touch <- pos_tail[[1]] %in% (pos_head[[1]] - 1):(pos_head[[1]] + 1) &
    pos_tail[[2]] %in% (pos_head[[2]] - 1):(pos_head[[2]] + 1)
  if (!touch) {
    if (df_moves$direction[[i]] == "u") {
      pos_tail[[1]] <- pos_head[[1]]
      pos_tail[[2]] <- pos_head[[2]] - 1
    } else if (df_moves$direction[[i]] == "d") {
      pos_tail[[1]] <- pos_head[[1]]
      pos_tail[[2]] <- pos_head[[2]] + 1
    } else if (df_moves$direction[[i]] == "l") {
      pos_tail[[1]] <- pos_head[[1]] + 1
      pos_tail[[2]] <- pos_head[[2]]
    } else if (df_moves$direction[[i]] == "r") {
      pos_tail[[1]] <- pos_head[[1]] - 1
      pos_tail[[2]] <- pos_head[[2]]
    }
  }
}

erg_vec |>
  bind_rows() |>
  distinct(pos_tail_x, pos_tail_y) |>
  nrow()
