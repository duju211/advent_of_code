solve_conflicts <- function(quiet) {
  conflicts_prefer(dplyr::filter, .quiet = quiet)
}