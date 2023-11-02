#' Counting Successes
#'
#' Count the number of success for a given value or predicate function.
#' * `num_success()` always returns a list column and should be `map`ped over outcomes.  Can be composed with other function/expressions inside a map.
#' * `num_success_int()`always returns an integer column and does not need to be `map`ped. Useful in creating a new integer column.
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
#' # Comparing to a atomic chr, returning a list
#' one.outcome %>% map(\(x) num_successes(x, 'H'))
#'
#' # Comparing to a atomic chr, returning a integer vector
#' one.outcome %>% map_int(\(x) num_successes(x, 'H'))
#'
#' # Same as above, simplified
#' one.outcome %>% num_successes_int('H')
#'
#' # Output is a list vector w/ same length as input list
#' one.outcome %>% map(\(x) num_successes(x, 'H'))
#'
#' # Output is a integer vector w/ same length as input list
#' one.outcome %>% num_successes_int('H') %>% str
#'
#' # walk(print) to inspect intermediate results
#' one.outcome %>% walk(print) %>% num_successes_int('H')
#'
#' # Compare using a predicate function
#' one.outcome %>% map_int(\(x) num_successes(x, \(t) t != 'H'))
#' one.outcome %>% num_successes_int(\(x) x != 'H')
#'
#' # Use `map` to generate a list of multiple outcomes
#' coin <- c('H', 'T')
#' outcomes <- map(1:5, \(i) sample(coin, 3, replace=TRUE))
#' outcomes
#'
#' # Comparing to a atomic chr
#' outcomes%>% num_successes_int('H')
#'
#' # walk(print) to inspect intermediate result
#' outcomes %>% walk(print) %>% num_successes_int('H')
#'
#' # Output is a list vector w/ same length as input list
#' outcomes %>% map(\(x) num_successes(x, 'H')) %>% str
#'
#' # Output is a integer vector w/ same length as input list
#' outcomes %>% num_successes_int('H') %>% str
#'
#' # Compare using a predicate function
#'  outcomes %>% map_int(\(x) num_successes(x, \(t) t != 'H'))
#'  outcomes %>% num_successes_int(\(x) x != 'H')
#'
#' # Composition.
#' # Use num_successes to, e.g., ask questions about the number of successes.
#' outcomes %>% walk(str) %>%  map_lgl(\(x) num_successes(x, 'H') > 1)
num_successes <-
  function(outcome, .p)
  {
    f <- if (rlang::is_function(.p)) {
      .p
    } else {
      function(x) x == .p
    }
    sum(f(outcome))
  }
#' @rdname num_successes
#' @export
num_successes_int <-
  function(col, .p)
  {
    f <- if (rlang::is_function(.p)) {
      .p
    } else {
      function(x) x == .p
    }
    (col
      %>% purrr::map_int(\(x) num_successes(x, f))
    )
  }

#' Counting Labels
#'
#' Counts the number of occurrences of a given label in a list column of
#' of character vectors, storing the result in a new column named "num_{.label}".
#' This method was designed for counting the outcomes in a multinomial setting.
#'
#' @param .data A data frame/tibble.
#' @param .col A list column of character variables.
#' @param .label The label being counted.
#'
#' @seealso [multicounts()]
#'
#' @return The original .data with a new count column
#' @export
#'
#' @examples
#' grades <- c('A', 'B', 'C', 'D', 'F')
#' probs <- c(0.23, 0.26, 0.21, 0.18, 0.12)
#' n <- 5
#'
#' replicate(5, sample(grades, n, prob = probs, replace = TRUE), .as = raw.grades) %>%
#'   get_label_count(raw.grades, 'A')
get_label_count <- function(.data, .col, .label) {
  .data %>% dplyr::mutate("num_{.label}" := num_successes_int({{.col}}, .label))}
#' Count Multiple Labels
#'
#' Counts the number of occurrences of a given collection of labels in a list column of
#' of character vectors, storing the result in a new columns named "num_{label}".
#' This method was designed for counting the outcomes in a multinomial setting.
#'
#' @param .data A data frame/tibble.
#' @param .col A list column of character variables.
#' @param .labels A vector of labels to be counted.
#'
#' @seealso [get_label_count()]
#'
#' @return The original .data with a new count columns, one for each entry in .labels
#' @export
#'
#'
#' @examples
#' grades <- c('A', 'B', 'C', 'D', 'F')
#' probs <- c(0.23, 0.26, 0.21, 0.18, 0.12)
#' n <- 5
#'
#' replicate(5, sample(grades, n, prob = probs, replace = TRUE), .as = raw.grades) %>%
#'   multicounts(raw.grades, grades)
multicounts <-
  function(.data, .col, .labels) {
    purrr::reduce(.labels, \(df, el) get_label_count(df, {{.col}}, el), .init = .data)
  }
