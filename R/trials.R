#' Initial a Probability Tableau
#'
#' Creates a `tibble` with `n` rows and a `.trial = 1:n` column
#'
#' @param n The number of trials.
#'
#' @return A `tibble` with `n` rows and a `.trial = 1:n` column.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#'
#' init_trials(10) # Blank tableau
#'
#' coin <- c('H', 'T')
#' init_trials(10) %>% mutate(.outcome = map(.trial, \(i) sample(coin, 5, replace = TRUE)))
init_trials <- function(n) {
  tidyr::tibble(.trial = seq(1, n, 1))
}


#' Title
#'
#' @param df A `data.frame`/`tibble` containing the experiment's parameters.
#' @param n  The number of trials.
#'
#' @return A `tibble` with the parameter space replicated `n` times and a `.trial=1:n` column.
#' @export
#'
#' @examples
#' library(tidyr)
#' tribble(
#'   ~coin, ~n,
#'   c('H', 'T'), 5,
#'   ) %>%
#'   add_trials(10)
add_trials <-
  function(df, n) {
    (df
     %>% dplyr::mutate(.trial = purrr::map(row.names(df), \(x) seq(1, n, 1)))
     %>% tidyr::unnest_longer(.trial)
    )
  }
