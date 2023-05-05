#' Counting Successes
#'
#' Count the number of success for a given value or predicate function.
#'
#'
#' @param col A list of length 1 containing a single outcome, either an atomic element or vector.
#' @param .p An atomic object to match or a Boolean predicate function.
#'
#' @return An integer representing the number of matching outcomes.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#'
#' # Make sure the outcome is stored in list of length 1
#' one.outcome <- list(c('H', 'H', 'T', 'H', 'T'))
#'
#' # Comparing to a atomic chr
#' one.outcome %>% num_successes('H')
#'
#' # walk(print) to inspect intermediate results
#' one.outcome %>% walk(print) %>% num_successes('H')
#'
#' # Output is a integer vector w/ same length as input list
#' one.outcome %>% num_successes('H') %>% str
#'
#' # Compare using a predicate function
#'  one.outcome %>% num_successes(\(x) x != 'H')
#'
#' # Use `map` to generate a list of multiple outcomes
#' coin <- c('H', 'T')
#' outcomes <- map(1:5, \(i) sample(coin, 3, replace=TRUE))
#'
#' # Comparing to a atomic chr
#' outcomes%>% num_successes('H')
#'
#' # walk(print) to inspect intermediate result
#' outcomes %>% walk(print) %>% num_successes('H')
#'
#' # Output is a integer vector w/ same length as input list
#' outcomes %>% num_successes('H') %>% str
#'
#' # Compare using a predicate function
#'  outcomes %>% num_successes(\(x) x != 'H')
num_successes <-
  function(col, .p)
  {
    f <- if (rlang::is_function(.p)) {
      .p
    } else {
      function(x) x == .p
    }
    (col
      %>% purrr::modify(f)
      %>% purrr::map_int(~purrr::reduce(.x, `+`, .init = 0))
    )
  }
