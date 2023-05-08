column_mapper <- function(func) {
  function(df, .f, ..., onto = .outcome, as = .outcome, .progress = FALSE) {
    onto <- dplyr::enquo(onto)
    as <- dplyr::enquo(as)
    df %>%
      dplyr::mutate(!!as := func(!!onto, .f, ..., .progress = .progress))
  }
}

#' Create a new column by applying a function each element of a `list` column
#'
#' The `col_map` functions transform an input column by applying a function to each element of a list column and returning a tibble containing new/updated column.:w
#' * `col_map()` always returns a `tibble` with a new/updated list column.
#' * `col_map_lgl()`, `col_map_int()`, `col_map_dbl()` and `col_map_chr()` return an new column with the the indicated type (or die trying). For these functions, `.f` must return a length-1 vector of the appropriate type.
#' * `col_map_vec()` simplifies to the common type of the output. It works with most types of simple vectors like Date, POSIXct, factors, etc.:w
#'
#'
#' @param df A `tibble` containing the target `list` column
#' @param col The target `list` column.
#' @param .f The functions to be mapped onto each element of `col`
#' @param ... Additional parameters passed to `.f`.  Use `...` to pass additional (constant) to `.f`.
#' @param as The name of the output column (default = .outcome)
#' @param .progress	 Whether to show a progress bar. Use TRUE to a turn on a basic progress bar, use a string to give it a name, or see `purrr::progress_bars` for more details.
#' @param .ptype If `NULL`, the default, the output type is the common type of the elements of the result. Otherwise, supply a "prototype" giving the desired type of output.
#'
#' @return A copy of the original `tibble` with newly added column.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#'
#' df <-
#'    tribble( ~coin, ~.trial,
#'            c('H', 'T'), 1:5) %>%
#'    unnest_longer(.trial)
#' df %>% glimpse
#'
#'
#' # By default, each map updates the `.outcome` column
#'
#' is.head <- function(x) x == 'H'
#'
#' out1 <- df %>%
#'   col_map(sample, size = 3, replace = TRUE,     # additional constant args follow function
#'           onto = coin) %>%                      # stored as default (.output)
#'   col_map(\(x) keep(x, is.head)) %>%            # updates default (.output)
#'   col_map_int(length)                           # updates default (.output)
#' out1 %>% glimpse
#'
#' # Use `onto` and `as` to specify new output columns
#'
#' is.head <- function(x) x == 'H'
#'
#' df %>%
#'   col_map(\(x) sample(x, 3, TRUE), onto = coin, as=toss) %>%
#'   col_map(\(x) keep(x, is.head), onto = toss, as = just.head) %>%
#'   col_map_int(length, onto=just.head, as=num.heads) -> out2
#' out2 %>% glimpse
col_map <- function(df, .f, ..., onto = .outcome, as = .outcome, .progress = FALSE) {
  onto <- dplyr::enquo(onto)
  as <- dplyr::enquo(as)
  df %>%
    dplyr::mutate(!!as := purrr::map(!!onto, .f, ..., .progress = .progress))
}
#' @rdname col_map
#' @export
# col_map_lgl <- column_mapper(purrr::map_lgl)
col_map_lgl <- function(df, .f, ..., onto = .outcome, as = .outcome, .progress = FALSE) {
  onto <- dplyr::enquo(onto)
  as <- dplyr::enquo(as)
  df %>%
    dplyr::mutate(!!as := purrr::map_lgl(!!onto, .f, ..., .progress = .progress))
}
#' @rdname col_map
#' @export
col_map_int <- column_mapper(purrr::map_int)
#' @rdname col_map
#' @export
col_map_dbl <- column_mapper(purrr::map_dbl)
#' @rdname col_map
#' @export
col_map_chr <- column_mapper(purrr::map_chr)
#' @rdname col_map
#' @export
#'
col_map_vec <- function(df, .f, ..., onto = .outcome, as = .outcome, .ptype=NULL, .progress = FALSE) {
  onto <- dplyr::enquo(onto)
  as <- dplyr::enquo(as)
  df %>%
    dplyr::mutate(!!as := purrr::map_vec(!!onto, .f, ..., .ptype=.ptype, .progress = .progress))
}
