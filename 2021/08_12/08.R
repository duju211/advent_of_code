library(tidyverse)

input <- read_lines("2021/08_12/input.txt")

numbers <- c("1" = 2, "4" = 4, "7" = 3, "8" = 7)

df_signals <- tibble(txt = input) |>
  separate(txt, into = c("signal", "output"), sep = " \\| ") |>
  separate(signal, into = str_glue("signal_{1:10}"), sep = "\\s") |>
  separate(output, into = str_glue("output_{1:4}"), sep = "\\s")

df_output <- df_signals |>
  select(starts_with("output"))

df_output |>
  pivot_longer(cols = everything()) |>
  mutate(value_length = str_length(value)) |>
  filter(value_length %in% numbers) |>
  nrow()
