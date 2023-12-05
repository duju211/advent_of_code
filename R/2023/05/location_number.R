location_number <- function(df_seed_map_complete, seed_nr) {
  df_look_up <- filter(df_seed_map_complete, source == "seed")
  
  while (unique(df_look_up$destination) != "location") {
    df_look_up_data <- filter(
      df_look_up, source_start <= seed_nr & source_end >= seed_nr)
    if (nrow(df_look_up_data) != 0) {
      seed_nr <- seed_nr - df_look_up_data$diff
    }
    df_look_up <- filter(
      df_seed_map_complete,
      source == unique(df_look_up$destination))
  }
  df_look_up_data <- filter(
    df_look_up, source_start <= seed_nr & source_end >= seed_nr)
  if (nrow(df_look_up_data) != 0) {
    seed_nr <- seed_nr - df_look_up_data$diff
  }
  return(seed_nr)
}