cube_conundrum_1 <- function(df_rounds, df_max_colors) {
  df_rounds |>
    left_join(df_max_colors, by = "color") |>
    group_by(game_nr) |>
    summarise(possible = all(anz <= max_anz))
}