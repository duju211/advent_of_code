trebuchet_2 <- function(input) {
  lines <- read_lines(input)
  
  num_txt <- c(
    "1|one" = "1", "2|two" = "2", "3|three" = "3", "4|four" = "4",
    "5|five" = "5", "6|six" = "6", "7|seven" = "7", "8|eight" = "8",
    "9|nine" = "9")
  
  df_lines <- tibble(line = lines)
  
  df_result_raw <- df_lines |>
    mutate(
      res = map(
        line,
        ~ .x |>
          str_locate_all(names(num_txt)) |>
          set_names(num_txt) |>
          map(~ tibble(start = .x[, "start"], end = .x[, "end"])) |>
          bind_rows(.id = "number")))
  
  df_result_raw |>
    unnest(res) |>
    arrange(start, end) |>
    group_by(line) |>
    summarise(
      first = first(number), last = last(number),
      result_number = parse_integer(str_c(first, last)))
}