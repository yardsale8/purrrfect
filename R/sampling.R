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
#' @param halt an integer limiting the length of the output.  Will be set to `length(x)` when `replace=FALSE`
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
#' sample_until(coin, 'H', replace = FALSE, prob=prob)
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
#' second.blue <- replicate(5, sample_until(urn, \(x) num_successes(x, 'Blue') == 2, replace=FALSE))
#' second.blue %>% glimpse
sample_until <-
  function(x, .p, replace = FALSE, prob = NULL, halt = 1000) {
    if (replace) {
      return(sample_until_replace(x, .p, prob, halt))

    } else {
      return(sample_until_no_replace(x, .p, prob))
    }
  }


