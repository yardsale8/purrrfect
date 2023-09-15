#' Row-wise parameter space creation
#'
#' Create tibbles of unstacked parameters using an easier to read row-by-row
#' layout. This is useful for creating an initial collection of parameters when
#' simulating a family of parametric distributions. Please see tibble-package
#' and `tribble` function for a general introduction.
#'
#' @param ... Arguments specifying the structure of a tibble.
#'   Variable names should be formulas, and may only appear before the data.
#'   These arguments are processed with rlang::list2() and support unquote via
#'   !! and unquote-splice via !!!.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' parameters(
#'    ~n,      ~p,
#'    c(5,10), c(0.4, 0.6))
parameters <-
  function(...) {
    tibble::tribble(...) %>% purrr::reduce(names(.), tidyr::unnest_longer, .init = .)
  }
