library(tidyverse)

df_sections_raw <- tibble(input = read_lines("2022/04_12/input")) |>
  separate(col = input, into = c("elf_1", "elf_2"), sep = ",") |>
  mutate(pair_id = row_number()) |>
  pivot_longer(cols = c(elf_1, elf_2)) |>
  separate(value, into = c("min", "max")) |>
  transmute(name, section = map2(min, max, ~ .x:.y), pair_id)

df_sections <- df_sections_raw |>
  pivot_wider(names_from = name, values_from = section)

# first 
df_result_1 <- df_sections |>
  filter(map2_lgl(elf_1, elf_2, ~ all(.x %in% .y) || all(.y %in% .x)))
nrow(df_result_1)

# second
df_result_2 <- df_sections |>
  filter(map2_lgl(elf_1, elf_2, ~ any(.x %in% .y) || any(.y %in% .x)))
nrow(df_result_2)