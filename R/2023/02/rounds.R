rounds <- function(df_games) {
  df_rounds <- df_games |>
    unnest(game) |>
    transmute(
      game_nr,
      round_txt = str_trim(game),
      red = str_extract(round_txt, "\\d+ red"),
      green = str_extract(round_txt, "\\d+ green"),
      blue = str_extract(round_txt, "\\d+ blue"),
      across(c(red, green, blue), parse_number))
  
  df_rounds |>
    pivot_longer(
      cols = c(red, green, blue), values_to = "anz", names_to = "color",
      values_drop_na = TRUE)
}