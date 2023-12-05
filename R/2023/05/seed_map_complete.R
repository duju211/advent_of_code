seed_map_complete <- function(df_seed_map) {
  df_seed_map |>
    mutate(
      across(c(destination_start, source_start, length), parse_number),
      source_end = source_start + length,
      diff = source_start - destination_start)
}