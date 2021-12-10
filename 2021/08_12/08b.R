library(tidyverse)

input <- read_lines("2021/08_12/input_ex.txt")

# 1: Nur 2 Buchsaben
# 2: 5 umgedreht
# 3
# 4
# 5: 6 ohne einen
# 6: 8 ohne a

# ###pos_1###
# pos_2#pos_3
# ###pos_4###
# pos_5#pos_6
# ###pos_7###

# pos_1 -> 7 ohne 1 -> "d"
# pos_2 -> 
# pos_3 -> 6 oder 9 Unterschied zur 8 -> "b"

df_numbers <- tibble(txt = c(
  "acedgfb: 8", "cdfbe: 5", "gcdfa: 2", "fbcad: 3", "dab: 7", "cefabd: 9",
  "cdfgeb: 6", "eafb: 4", "cagedb: 0", "ab: 1")) |>
  separate(txt, into = c("digits", "number"), sep = ": ") |>
  mutate(digits = map(digits, ~ str_split(.x, "")[[1]]))

df_signal_output <- tibble(txt = input) |>
  separate(txt, into = c("signal", "output"), sep = " \\| ") |>
  separate(signal, into = str_glue("signal_{1:10}"), sep = "\\s") |>
  separate(output, into = str_glue("output_{1:4}"), sep = "\\s")

df_signal <- df_signal_output |>
  select(starts_with("signal")) |>
  mutate(id = row_number()) |>
  pivot_longer(cols = starts_with("signal")) |>
  mutate(digits = str_split(value, ""), anz_digits = map_int(digits, length)) |>
  nest(signal = -id)

decode_digits <- function(test) {
  dig_1 <- test$digits[test$anz_digits == 2][[1]]
  dig_7 <- test$digits[test$anz_digits == 3][[1]]
  dig_4 <- test$digits[test$anz_digits == 4][[1]]
  dig_8 <- test$digits[test$anz_digits == 7][[1]]
  
  dig_3 <- test |>
    filter(anz_digits == 5) |>
    filter(map_lgl(digits, ~ all(dig_1 %in% .x))) |>
    pull(digits) |>
    first()
  
  dig_9 <- union(dig_3, dig_4)
  dig_0 <- test |>
    filter(
      anz_digits == 6, !map_lgl(digits, ~ all(.x %in% dig_9)),
      map_lgl(digits, ~ all(dig_1 %in% .x))) |>
    pull(digits) |>
    first()
  dig_6 <- test |>
    filter(
      anz_digits == 6,
      map_lgl(digits, ~ all(.x %in% dig_9) | all(.x %in% dig_0))) |>
    pull(digits) |>
    first()
  
  dig_5 <- test |>
    filter(
      anz_digits == 5, !map_lgl(digits, ~ all(.x %in% dig_3)),
      map_lgl(digits, ~ length(union(dig_9, .x)) == length(dig_9))) |>
    pull(digits) |>
    first()
  dig_2 <- test |>
    filter(
      anz_digits == 5,
      !map_lgl(digits, ~ all(.x %in% dig_3) || all(.x %in% dig_5))) |>
    pull(digits) |>
    first()
  
  tibble(
    number = 0:9,
    dec_digit = list(
      dig_0, dig_1, dig_2, dig_3, dig_4, dig_5, dig_6, dig_7, dig_8, dig_9))
}

df_signal_decoded <- df_signal |>
  transmute(id, decoded_signal = map(signal, decode_digits))

df_output <- df_signal_output |>
  select(starts_with("output")) |>
  mutate(id = row_number()) |>
  pivot_longer(cols = starts_with("output")) |>
  mutate(digits = str_split(value, ""))

df_output |>
  left_join(df_signal_decoded) |>
  unnest(decoded_signal) |>
  filter(map2_lgl(digits, dec_digit, ~ setequal(.y, .x)))

test$dec_digit[[2]]
test$dec_digit[[3]]
         