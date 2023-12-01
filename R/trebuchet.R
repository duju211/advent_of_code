trebuchet <- function(input) {
  lines <- read_lines(input)
  
  tibble(line = lines) |>
    mutate(number = str_extract_all(line, "\\d")) |>
    unnest(number) |>
    group_by(line) |>
    summarise(first = first(number), last = last(number)) |>
    mutate(result_number = parse_integer(str_c(first, last)))
}