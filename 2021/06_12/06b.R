library(tidyverse)

#input <- "3,4,3,1,2"
input <- read_lines("2021/06_12/input.txt")
days <- 257

df_swarm_init <- tibble(
  txt = input) |>
  transmute(int_timer = str_split(txt, ",")) |>
  unnest(int_timer) |>
  count(int_timer) |>
  mutate(int_timer = as.integer(int_timer))

df_swarm <- tibble(
  int_timer = -1:8) |>
  left_join(df_swarm_init, by = "int_timer") |>
  replace_na(list(n = 0L))

erg_vec <- vector(mode = "list", length = days)
erg_vec[[1]] <- df_swarm

process_swarm <- function(df_swarm) {
  df_swarm_pro <- df_swarm |>
    mutate(n = lead(n, default = 0))
  
  if (df_swarm_pro$n[df_swarm_pro$int_timer == -1] != 0) {
    df_swarm_pro$n[df_swarm_pro$int_timer == 6] <-
      df_swarm_pro$n[df_swarm_pro$int_timer == 6] +
      df_swarm_pro$n[df_swarm_pro$int_timer == -1]
    df_swarm_pro$n[df_swarm_pro$int_timer == 8] <-
      df_swarm_pro$n[df_swarm_pro$int_timer == 8] +
      df_swarm_pro$n[df_swarm_pro$int_timer == -1]
    df_swarm_pro$n[df_swarm_pro$int_timer == -1] <- 0
  }
  
  df_swarm_pro
}

for (i in 2:days) {
  erg_vec[[i]] <- process_swarm(erg_vec[[i - 1]])
}

erg_vec[[length(erg_vec)]] |>
  summarise(erg = sum(n))