#' Random Samples and Permutations
#'
#' @inheritParams stringr::str_split
#'
#' @return A list of outcomes
#' @export
#'
#' @examples
take.one.sample <-
  function(x, size, replace = FALSE, prob=NULL) {
    list(sample(x, size, replace, prob))
    }
