source("setup.R")

purrr::walk(fs::dir_ls("R", recurse = TRUE, type = "file"), source)

list(
  #day1
  tar_target(df_trebuchet_train, trebuchet("2023/01/input_train")),
  tar_target(df_trebuchet_test, trebuchet("2023/01/input_test")),
  tar_target(df_trebuchet_2_train, trebuchet_2("2023/01/input2_train")),
  tar_target(df_trebuchet_2_test, trebuchet_2("2023/01/input_test")),
  
  #day_2
  tar_target(
    df_max_colors, tibble(
      color = c("red", "green", "blue"), max_anz = c(12, 13, 14))),
  tar_target(input_train_day2_1, read_lines("2023/02/input_1_train")),
  tar_target(input_test_day2_1, read_lines("2023/02/input_1_test")),
  tar_target(df_games_train, games(input_train_day2_1)),
  tar_target(df_rounds_train, rounds(df_games_train)),
  tar_target(df_games_test, games(input_test_day2_1)),
  tar_target(df_rounds_test, rounds(df_games_test)),
  tar_target(
    df_cube_conundrum_1_train,
    cube_conundrum_1(df_rounds_train, df_max_colors)),
  tar_target(
    df_cube_conundrum_1_test,
    cube_conundrum_1(df_rounds_test, df_max_colors)),
  tar_target(
    df_cube_conundrum_2_train,
    cube_conundrum_2(df_rounds_train)),
  tar_target(df_cube_conundrum_2_test, cube_conundrum_2(df_rounds_test)),
  
  tar_render(advent_2023, "advent_2023.Rmd")) |>
  tar_hook_before(solve_conflicts(quiet = TRUE))