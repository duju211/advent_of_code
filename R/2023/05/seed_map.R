seed_map <- function(input) {
  tibble(line = input) |>
    mutate(
      source = map_chr(str_split(line, "-to-|\\s"), 1),
      destination = map_chr(
        str_split(line, "-to-|\\s"), possibly(~ .x[[2]], NA_character_))) |>
    mutate(
      across(
        c(source, destination),
        ~ if_else(str_detect(.x, "^[a-z]+$"), .x, NA_character_))) |>
    fill(source, destination) |>
    filter(str_detect(line, "\\d+\\s\\d+\\s\\d+"), !is.na(source)) |>
    separate(line, into = c("destination_start", "source_start", "length"))
}