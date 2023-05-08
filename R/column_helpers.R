#' Count Successes in a `list` Column
#'
#' Create a new `integer` column contain the number of successes in corresponding entry of a list column of outcomes.
#'
#' @param df A `tibble` containing the outcome column.
#' @param col A `list` column of outcomes to inspect.
#' @param .p An atomic object to match or a Boolean predicate function.
#' @param name Name of new column (default = .successes)
#'
#' @return A `tibble` with all original columns and a new column of success counts.
#' @export
#'
#' @examples
#' library(tidyr)
#' library(purrr)
#' library(dplyr)
#'
#' df <- tibble(lengths = 1:5) %>%
#'    mutate(.outcome = map(lengths, \(n) rep_len('H', n))) %>%
#'    walk(glimpse)
#'
#' df %>%
#'  col_num_successes(.outcome, 'H') %>%
#'  glimpse
col_num_successes <- function(df, col, .p, name = .successes) {
  name <- dplyr::enquo(name)
  col <- dplyr::enquo(col)

  (df
    %>% dplyr::mutate(!!name := num_successes_int(!!col, .p)))
}
