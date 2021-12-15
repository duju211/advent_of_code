library(tidyverse)
library(glue)

input <- read_lines("2021/14_12/input_ex.txt")

poly_template_letters <- str_split(input[[1]], "")[[1]]
nr_steps <- 40L

poly_template <- vector(
  mode = "character", length = length(poly_template_letters) - 1L)
for (i in seq_along(poly_template)) {
  poly_template[[i]] <- glue_collapse(
    c(poly_template_letters[i], poly_template_letters[i + 1L]))
}

df_poly_template <- tibble(temp = poly_template)

df_insertion_rules <- tibble(txt = input[3:length(input)]) |>
  separate(txt, into = c("temp", "new_temp"), sep = " -> ")

for (i in seq_len(nr_steps)) {
  df_poly_template <- df_poly_template |>
    left_join(df_insertion_rules, by = "temp") |>
    mutate(
      new_string = case_when(
        row_number() == max(row_number()) ~ str_glue(
          "{str_sub(temp, end = 1)}{new_temp}{str_sub(temp, start = 2)}"),
        TRUE ~ str_glue("{str_sub(temp, end = 1)}{new_temp}"))) |>
    summarise(temp = glue_collapse(new_string)) |>
    mutate(split = list(1:(str_length(temp) - 1L))) |>
    unnest(split) |>
    transmute(temp = str_sub(temp, split, split + 1L))
}

df_poly_template |>
  separate(temp, into = c("first_letter", "second_letter"), sep = c(1, 2)) |>
  mutate(row_nr = row_number()) |>
  pivot_longer(
    cols = c(first_letter, second_letter),
    names_to = "letter_nr", values_to = "letter") |>
  mutate(rel = !(row_nr != 1 & letter_nr != "second_letter")) |>
  filter(rel) |>
  count(letter, sort = TRUE) |>
  summarise(res = first(n) - last(n))
