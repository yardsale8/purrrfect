#' A Better `replicate`
#'
#' The function `replicate`, along with the family of `replicate_*`
#' functions, are meant replace `base::replicate`
#' * `replicate()` always returns the output in a list column.
#' * `replicate_lgl()` , `replicate_int()` , `replicate_dbl()` and `replicate_chr()` use the `map_*` functions from `purrr` to return an atomic vector of the indicated type (or die trying). For these functions, `expr` must return a length-1 vector of the appropriate type.
#' * `replicate_vec()` uses `purrr::map_vec` to simplifies to the common type of the output. It works with most types of simple vectors like Date, POSIXct, factors, etc.
#'
#' The output of `base::replicate` (with default parameters) is tricky
#' to use: The returned object is either a vector or list, depending on if the
#' `expr` generates an atomic element or vector/list, respectively.
#' Consequently, you need to transpose the matrix before converting to a `data.frame`
#' for expressions that result in a vector/list.
#'
#' On the other hand, this suite of functions returns a `tibble` containing
#' the results (as a list column), along with a `.trial` field.
#'
#' Similar to `base::replicate` the `expr` re-evaluated on each trial,
#' e.g., to facilitate replicating expressions with random output.
#'
#'
#'
#'
#'
#' @param n The number of replications.
#' @param expr The expression to replicates.
#'
#' @return A `tibble` containing the results in a field names `.outcome`, along with a `.trial` field containing the replication number.
#' @export
#'
#' @examples
#' library(dplyr)
#' # Use `replicate` to put compound output in a list column
#' out <- replicate(10, sample(x = c('H', 'T'), size = 5, replace = TRUE))
#' str(out)
#' glimpse(out)
#'
#' # Use `replicate_*` to specify the output for simple expression returning a single object
#' out <- replicate_int(10, rbinom(1, 5, 0.5))
#' str(out)
#' glimpse(out)
#'
#' # Use `replicate_vec` to specify the output for expressions returning simple vectors like Date, POSIXct, factors, etc.
#' out <- replicate_vec(10, as.Date("01-01-01"))
#' str(out)
#' glimpse(out)
replicate <- function(n,
                      expr,
                      .reshape = NULL,
                      names_sep = '.',
                      transform = NULL,
                      .as = .outcome
                      ) {
  f <- eval.parent(substitute(function(...) expr))
  (
    init_trials(n)
    %>% dplyr::mutate('{{.as}}' := purrr::map(.trial, \(i) f()))
    %>% reshape_and_transform(.outcome_col = {{.as}},
                              .reshape = .reshape,
                              names_sep = names_sep,
                              transform = transform)
  )
}
#' @rdname replicate
#' @export
replicate_lgl <- function(n, expr,.as = .outcome) {
  f <- eval.parent(substitute(function(...) expr))
  (
    init_trials(n)
    %>% dplyr::mutate(.outcome = purrr::map_lgl(.trial, \(i) f()))
  )
}
#' @rdname replicate
#' @export
replicate_int <- function(n, expr,.as = .outcome) {
  f <- eval.parent(substitute(function(...) expr))
  (
    init_trials(n)
    %>% dplyr::mutate(.outcome = purrr::map_int(.trial, \(i) f()))
  )
}
#' @rdname replicate
#' @export
#'
replicate_dbl <- function(n, expr,.as = .outcome) {
  f <- eval.parent(substitute(function(...) expr))
  (
    init_trials(n)
    %>% dplyr::mutate('{{.as}}' := purrr::map_dbl(.trial, \(i) f()))
  )
}
#' @rdname replicate
#' @export
replicate_chr <- function(n, expr,.as = .outcome) {
  f <- eval.parent(substitute(function(...) expr))
  (
    init_trials(n)
    %>% dplyr::mutate('{{.as}}' := purrr::map_chr(.trial, \(i) f()))
  )
}
#' @rdname replicate
#' @export
replicate_vec <- function(n, expr,.as = .outcome) {
  f <- eval.parent(substitute(function(...) expr))
  (
    init_trials(n)
    %>% dplyr::mutate('{{.as}}' := purrr::map_vec(.trial, \(i) f()))
  )
}
#' @rdname replicate
#' @export
replicate2 <- function(n,
                      expr1,
                      expr2,
                      .as1 = .outcome1,
                      .as2 = .outcome2
                      ) {
  f1 <- eval.parent(substitute(function(...) expr1))
  f2 <- eval.parent(substitute(function(...) expr2))
  (
    init_trials(n)
    %>% dplyr::mutate('{{.as1}}' := purrr::map(.trial, \(i) f1()))
    %>% dplyr::mutate('{{.as2}}' := purrr::map(.trial, \(i) f2()))
  )
}
#' @rdname replicate
#' @export
replicate2_lgl <- function(n, expr1, expr2, .as1 = .outcome1, .as2 = .outcome2) {
  f1 <- eval.parent(substitute(function(...) expr1))
  f2 <- eval.parent(substitute(function(...) expr2))
  (
    init_trials(n)
    %>% dplyr::mutate('{{.as1}}' := purrr::map_lgl(.trial, \(i) f1()))
    %>% dplyr::mutate('{{.as2}}' := purrr::map_lgl(.trial, \(i) f2()))
  )
}
#' @rdname replicate
#' @export
replicate2_int <- function(n, expr1, expr2, .as1 = .outcome1, .as2 = .outcome2) {
  f1 <- eval.parent(substitute(function(...) expr1))
  f2 <- eval.parent(substitute(function(...) expr2))
  (
    init_trials(n)
    %>% dplyr::mutate('{{.as1}}' := purrr::map_int(.trial, \(i) f1()))
    %>% dplyr::mutate('{{.as2}}' := purrr::map_int(.trial, \(i) f2()))
  )
}
#' @rdname replicate
#' @export
#'
replicate2_dbl <- function(n, expr1, expr2, .as1 = .outcome1, .as2 = .outcome2) {
  f1 <- eval.parent(substitute(function(...) expr1))
  f2 <- eval.parent(substitute(function(...) expr2))
  (
    init_trials(n)
    %>% dplyr::mutate('{{.as1}}' := purrr::map_dbl(.trial, \(i) f1()))
    %>% dplyr::mutate('{{.as2}}' := purrr::map_dbl(.trial, \(i) f2()))
  )
}
#' @rdname replicate
#' @export
replicate2_chr <- function(n, expr1, expr2, .as1 = .outcome1, .as2 = .outcome2) {
  f1 <- eval.parent(substitute(function(...) expr1))
  f2 <- eval.parent(substitute(function(...) expr2))
  (
    init_trials(n)
    %>% dplyr::mutate('{{.as1}}' := purrr::map_chr(.trial, \(i) f1()))
    %>% dplyr::mutate('{{.as2}}' := purrr::map_chr(.trial, \(i) f2()))
  )
}
#' @rdname replicate
#' @export
replicate2_vec <- function(n, expr1, expr2, .as1 = .outcome1, .as2 = .outcome2) {
  f1 <- eval.parent(substitute(function(...) expr1))
  f2 <- eval.parent(substitute(function(...) expr2))
  (
    init_trials(n)
    %>% dplyr::mutate('{{.as1}}' := purrr::map_vec(.trial, \(i) f1()))
    %>% dplyr::mutate('{{.as2}}' := purrr::map_vec(.trial, \(i) f2()))
  )
}
