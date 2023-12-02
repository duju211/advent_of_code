games <- function(input) {
  tibble(line = input) |>
    mutate(
      game_nr = parse_number(str_extract(line, "Game \\d+")),
      game_string = str_trim(map_chr(str_split(line, ":"), 2)),
      game = str_split(game_string, ";")) |>
    select(game_nr, game_string, game)
}