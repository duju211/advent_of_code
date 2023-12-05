extract_seeds <- function(input) {
  input |>
    str_subset("^seeds:") |>
    str_extract_all("\\d+") |>
    first() |>
    parse_number()
}