library(tidyverse)

#input <- "3,4,3,1,2"
input <- read_lines("2021/06_12/input.txt")
days <- 81

df_init_swarm <- tibble(
  txt = input) |>
  transmute(int_timer = str_split(txt, ",")) |>
  unnest(int_timer) |>
  mutate(fish_nr = row_number(), int_timer = as.integer(int_timer))

erg_vec <- vector(mode = "list", length = days)
erg_vec[[1]] <- df_init_swarm

process_swarm <- function(df_swarm) {
  df_swarm_pro <- df_swarm |>
    mutate(int_timer = int_timer - 1)
  
  df_swarm_rep <- df_swarm_pro |>
    filter(int_timer < 0) |>
    mutate(int_timer = 6)
  
  df_swarm_rep |>
    bind_rows(
      mutate(
        df_swarm_rep,
        int_timer = 8, fish_nr = max(df_swarm$fish_nr) + row_number())) |>
    bind_rows(anti_join(df_swarm_pro, df_swarm_rep, by = "fish_nr")) |>
    arrange(fish_nr)
}

for (i in 2:length(erg_vec)) {
  erg_vec[[i]] <- process_swarm(erg_vec[[i - 1]])
}

nrow(erg_vec[[length(erg_vec)]])
