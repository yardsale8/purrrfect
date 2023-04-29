#' Random Samples and Permutations
#'
#' @inheritParams base::sample
#'
#' @return A list of outcomes
#' @export
#'
#' @examples
take.one.sample <-
  function(x, size, replace = FALSE, prob=NULL) {
    list(sample(x, size, replace, prob))
    }
