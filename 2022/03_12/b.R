library(tidyverse)

df_prio <- bind_rows(
  tibble(item = letters, priority = 1:26),
  tibble(item = LETTERS, priority = 27:52))

df_backpack_raw <- tibble(input = read_lines("2022/03_12/input")) |>
  mutate(backpack_nr = row_number())

df_backpack <- df_backpack_raw |>
  transmute(
    group = (backpack_nr - 1) %/% 3, items = str_split(input, ""))

df_result <- df_backpack |>
  group_by(group) |>
  summarise(item = reduce(items, intersect)) |>
  left_join(df_prio, by = "item")

pull(df_result, priority) |> sum()