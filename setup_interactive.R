source(here::here("setup.R"))
purrr::walk(tar_option_get("packages"), library, character.only = TRUE)
walk(dir_ls(here::here("R"), type = "file", recurse = TRUE), source)
solve_conflicts(quiet = FALSE)
