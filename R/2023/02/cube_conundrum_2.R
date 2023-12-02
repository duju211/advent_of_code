cube_conundrum_2 <- function(df_rounds) {
  df_rounds |>
    group_by(game_nr, color) |>
    summarise(min_anz = max(anz), .groups = "drop") |>
    pivot_wider(names_from = color, values_from = min_anz) |>
    mutate(power = red * blue * green)
}