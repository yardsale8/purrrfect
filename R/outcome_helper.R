#' Count the number of successes
#'
#' @param col A list of outcomes.
#' @param p Either an atomic element or predicate functions.
#'
#' @return The number of successes.
#' @export
#'
#' @examples
#' library(dplyr)
#' x <- list(c('Red', 'Green', 'Blue'))
#' x %>% num.successes('Red')
#' x %>% num.successes(\(x) x != 'Green')
num.successes <-
  function(col, p)
  {
    f <- if (is.function(p)) {
      p
    } else {
      function(x) x == p
    }
    purrr::map_int(col, ~length(purrr::keep(., f)))
  }
