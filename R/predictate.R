#' Predicate functions
#'
#' Predicate comparison functions in plain English.
#'
#' @param x atomic vectors, symbols, calls, or other objects for which methods have
#' @param y been written
#'
#' @return Boolean
#' @export
#'
#' @examples
#' library(dplyr)
#' x <- stats::rnorm(20)
#' x %>% strictly.less(1)
#' x[x %>% strictly.less(1)]

is.equal <- function(x, y) {
  x == y
}
#' @rdname is.equal
#' @export
not.equal <- `!=` #function(x, y) x != y
#' @rdname is.equal
#' @export
strictly.greater <- function(x, y) x > y
#' @rdname is.equal
#' @export
at.least <- function(x, y) x >= y
#' @rdname is.equal
#' @export
strictly.less <- function(x, y) x < y
#' @rdname is.equal
#' @export
at.most <- function(x, y) x <= y
