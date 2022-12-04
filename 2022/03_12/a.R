library(tidyverse)

df_prio <- bind_rows(
  tibble(item = letters, priority = 1:26),
  tibble(item = LETTERS, priority = 27:52))

df_backpack_raw <- tibble(input = read_lines("2022/03_12/a/input_ex")) |>
  mutate(
    backpack_nr = row_number(),
    length = str_length(input),
    comp_1 = str_sub(input, end = length / 2),
    comp_2 = str_sub(input, start = (length / 2) + 1))

df_backpack <- df_backpack_raw |>
  pivot_longer(
    cols = c(comp_1, comp_2), values_to = "content", names_to = "comp_nr") |>
  transmute(backpack_nr, comp_nr, items = str_split(content, ""))

df_result <- df_backpack |>
  pivot_wider(names_from = comp_nr, values_from = items) |>
  mutate(wrong_item = map2_chr(comp_1, comp_2, intersect)) |>
  left_join(df_prio, by = c("wrong_item" = "item"))

pull(df_result, priority) |> sum()
