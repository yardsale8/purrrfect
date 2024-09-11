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

get_success_func <-
  function(.p) {
    .f <- if (rlang::is_function(.p)) {
      .p
    } else {
      function(x) sum(x == .p) == 1
    }
    return(.f)
  }

sample_until_replace <-
  function(x, .p, prob = NULL, halt = 1000)
  {
    .f <- get_success_func(.p)
    out <- base::sample(x, size=1, prob = prob, replace=TRUE)
    n <- 1

    while(!.f(out) & n < halt) {
      n <- n + 1
      out[n] <- base::sample(x, size=1, prob = prob, replace=TRUE)
    }

    return(out)
  }

sample_until_no_replace <-
  function(x, .p, prob = NULL) {
    .f <- get_success_func(.p)
    perm <- base::sample(x, replace = FALSE, prob = prob)
    halt <- length(x)
    n <- 1
    out <- perm[1:n]

    while(!.f(out) & n < halt) {
      n <- n + 1
      out <- perm[1:n]
    }
    return(out)
  }

#' Sample until a given condition
#'
#' Take a sample until meeting some user-provided condition is met.
#'
#' @param x Either a vector of one more more elements, or a positive integer.
#' @param .p Either an atomic element or a Boolean predicate function.
#' @param replace Whether to sample with replacement.
#' @param prob a vector of weigths for obtaining the element of `x`
#' @param .halt an integer limiting the length of the output.  Will be set to `length(x)` when `replace=FALSE`
#'
#' @return The smallest sample that either (A) matches `.p` when `.p` is atomic, (B) satisfies the predicate `.p`, (C) has reached the halting length.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Flipping a fair coin
#' coin <- c('H', 'T')
#' sample_until(coin, 'H', replace = FALSE)
#' first.head <- replicate(5, sample_until(coin, 'H', replace = TRUE))
#' first.head %>% glimpse
#' three.heads <- \(x) num_successes(x, 'H') == 3
#' third.head <- replicate(5, sample_until(coin, three.heads, replace = TRUE))
#' third.head %>% glimpse
#'
#' # Flipping a biased coin
#' coin <- c('H', 'T')
#' prob <- c(0.6, 0.4)
#' sample_until(coin, 'H', replace = TRUE, prob=prob)
#' first.head <- replicate(5, sample_until(coin, 'H', replace = TRUE, prob=prob))
#' first.head %>% glimpse
#' three.heads <- \(x) num_successes(x, 'H') == 3
#' third.head <- replicate(5, sample_until(coin, three.heads, replace = TRUE, prob=prob))
#' third.head %>% glimpse
#'
#' # Sampling without replacement
#' urn <- c(rep_len('Blue', 10), rep_len('White', 5))
#' urn
#'
#' sample_until(urn, 'Blue', replace=FALSE)
#'
#' first.blue <- replicate(5, sample_until(urn, 'Blue', replace=FALSE))
#' first.blue %>% glimpse
#'
#' two.blue <- \(x) num_successes(x, 'Blue') == 2
#' second.blue <- replicate(5, sample_until(urn, two.blue, replace=FALSE))
#' second.blue %>% glimpse
sample_until <-
  function(x, .p, replace = FALSE, prob = NULL, .halt = 1000) {
    if (replace) {
      return(sample_until_replace(x, .p, prob, .halt))

    } else {
      return(sample_until_no_replace(x, .p, prob))
    }
  }

#' Sample from a List Column
#'
#' Take a sample from a list column `.col` from the tibble `.df`
#'
#' @param .df A tibble containing the sample space in a column and one row per trial.
#' @param .col The list column to be sampled.
#' @param size a non-negative integer giving the number of items to choose.
#' @param prob a vector of probability weights for obtaining the elements of the vector being sampled.
#' @param replace should sampling be with replacement?
#' @param .as the name of the resulting outcome column (default = .outcome)
#'
#' @return An updated tibble containing the outcomes in a list column
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Flip a fair coin 3 times (10 trials)
#' tribble(~coin,
#'         c('H', 'T')) %>%
#'   add_trials(10) %>%
#'   col_sample(coin, 3, replace = TRUE)
#'
#' # Same experiment, but rename the outcome column
#' tribble(~coin,
#'         c('H', 'T')) %>%
#'    add_trials(10) %>%
#'    col_sample(coin, 3, replace = TRUE, .as = three.tosses)
col_sample <-
  function(.df, .col, size, prob = NULL, replace = FALSE, .as = .outcome) {
    ( .df
      %>% dplyr::mutate("{{.as}}":= purrr::map({{.col}}, \(x) sample(x, size, prob = prob, replace = replace )))
    )
  }

#' Random Samples of a Dataframe
#'
#' `sample_table` samples the rows of `.df` returning the resulting data frame
#'
#' @param .df A `data.frame`/`tibble`
#' @param size A positive number representing the number of rows to be sampled.
#' @param replace should sampling be with replacement?
#' @param prob a vector of probability weights for obtaining the elements of the vector being sampled.  Can either be a column of weights contained in `.df.` or a separate vector with a length of `nrow(.df)`
#'
#' @return A `data.frame`/`tibble`
#' @export
#'
#' @examples
#' library(purrr)
#'
#' biased_coin <- data.frame(toss = c(rep('H', 55), rep('T', 45)))
#'
#' sample_table(biased_coin, 5, replace = TRUE)
#'
#' map(1:10, \(i) sample_table(biased_coin, 5, replace = TRUE))
#'
#'
#' urn <- data.frame(draw = c(rep('R', 4), rep('W', 3)))
#' urn
#'
#' sample_table(urn, 3, replace = FALSE)
#'
#' coin <- data.frame(toss = c('H', 'T'))
#' ps <- c(0.55, 0.45)
#' sample_table(coin, 3, replace = TRUE, prob = ps)
#'
#' coin_w_weights <- data.frame(toss = c('H', 'T'), weight = c(0.55, 0.45))
#' coin_w_weights
#'
#' sample_table(coin_w_weights, 3, replace = TRUE, prob = weight)
sample_table <-
  function(.df, size, replace = FALSE, prob = NULL) {
    prob <- dplyr::enquo(prob)
    sample_index_quo = dplyr::quo(sample(1:nrow(.df), size, replace = replace, prob = !!prob))
    sample_index = rlang::eval_tidy(sample_index_quo, .df)
    return(.df[sample_index, ])
  }

