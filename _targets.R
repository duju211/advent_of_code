source("setup.R")

purrr::walk(fs::dir_ls("R"), source)

list(
  #day1
  tar_target(df_trebuchet_train, trebuchet("2023/01/input_train")),
  tar_target(df_trebuchet_test, trebuchet("2023/01/input_test")),
  tar_target(df_trebuchet_2_train, trebuchet_2("2023/01/input2_train")),
  tar_target(df_trebuchet_2_test, trebuchet_2("2023/01/input_test")),
  
  tar_render(advent_2023, "advent_2023.Rmd")) |>
  tar_hook_before(solve_conflicts(quiet = TRUE))