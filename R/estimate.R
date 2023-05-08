# See this answer for making indicator columns: https://stackoverflow.com/a/53714125/2347296
# See this post on passing column names: https://stackoverflow.com/questions/48062213/using-column-names-as-function-arguments


#' Construct tables of probability estimates
#'
#'  Estimate probabilities  with `tabulate` by using
#' * `df %>% tabulate(x)` estimate the marginal distribution of `x`
#' * `df %>% group_by(y) %>% tabulate(x)` to estimate the conditional distributions of `x` given `y`
#'
#' @param df A `tibble` containing one or more variables.
#' @param col The column of interest
#' @param prefix The string used as a prefix in the output.
#'
#' @return A table of (row-wise) relative frequencies.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#'
#' coin <- c('H', 'T')
#' prob <- c(0.55, 0.45)
#'
#' # Getting estimated the marginal distribution
#' init_trials(1000) %>%
#'   mutate(one.toss = map(.trial, \(i) sample(coin, 1, prob = prob))) %>%
#'   tabulate(one.toss)
#'
#' # Estimating the conditional distributions
#' init_trials(1000) %>%
#'   mutate(three.tosses = map(.trial, \(i) sample(coin, 3, replace=TRUE, prob = prob)),
#'          first.toss = map_chr(three.tosses, \(x) x %>% pluck(1)),
#'          second.toss = map_chr(three.tosses, \(x) x %>% pluck(2)),
#'         ) %>%
#'   group_by(first.toss) %>%
#'   tabulate(second.toss, prefix = "second.toss=")
tabulate <- function(df, col, prefix = 'X = ') {
  myenc <- dplyr::enquo(col)
  (df
    %>% dplyr::mutate(.unique_row_id = 1:dplyr::n())
    %>% dplyr::mutate(dummy = 1)
    %>% dplyr::mutate(labels := paste0(prefix, !!myenc))
    %>% dplyr::select(.unique_row_id, labels, dummy)
    %>% tidyr::spread(labels, dummy, fill = 0)
    %>% dplyr::select(-.unique_row_id)
    %>% dplyr::summarise_all(mean)
  )
}

#' Estimate probabilities
#'
#'  Estimate probabilities for a Boolean/logical vector `x` by using
#' * `df %>% estimate_prob(x)` estimate the marginal probability estimate of `x==TRUE`
#' * `df %>% group_by(y) %>% estimate_prob(x)` to estimate the conditional probabilities of `x=TRUE` given `y`
#'
#' @param df A `data.frame`/`tibble`
#' @param col A Boolean/logical column
#'
#' @return A table containing the estimated probability.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#'
#' coin <- c('H', 'T')
#' prob <- c(0.55, 0.45)
#'
#' #Example 1.  Estimate P(First is H)
#' init_trials(1000) %>%
#'    mutate(three.tosses = map(.trial, \(i) sample(coin, 3, replace=TRUE, prob = prob)),
#'           first.head = map_lgl(three.tosses, \(x) pluck(x, 1) == 'H'),
#'    ) -> df
#'
#' df %>% glimpse # Or use `View(df)` to see the whole tableau
#'
#' df %>%
#'   estimate_prob(first.head)
#'
#' # Package the whole experiment in one pipe
#'
#' (init_trials(1000)
#'    %>% mutate(three.tosses = map(.trial, \(i) sample(coin, 3, replace=TRUE, prob = prob)),
#'           first.head = map_lgl(three.tosses, \(x) pluck(x, 1) == 'H'),
#'           )
#'    # %>% walk(glimpse) #uncomment to inspect first few rows of tableau
#'    %>% estimate_prob(first.head)
#'  )
#'
#'  # Example 2. Estimate P(first is H | second is T)
#' (init_trials(1000)
#'    %>% mutate(three.tosses = map(.trial, \(i) sample(coin, 3, replace=TRUE, prob = prob)),
#'               first.head = map_lgl(three.tosses, \(x) pluck(x, 1) == 'H'),
#'               second.tail = map_lgl(three.tosses, \(x) pluck(x, 2) == 'T'),
#'               )
#'    # %>% walk(glimpse) #uncomment to inspect first few rows of tableau
#'    %>% group_by(second.tail)
#'    %>% estimate_prob(first.head)
#' )
estimate_prob<- function(df, col) {
  col <- dplyr::enquo(col)
  df %>%
    dplyr::summarise(!!col := mean(!!col, na.rm = TRUE))
}

#' Estimate all probabilities
#'
#'  Estimate probabilities for all Boolean/logical vectors in `df` by using
#' * `df %>% estimate_all_prob` to estimate all marginal probabilities
#' * `df %>% group_by(y) %>% estimate_all_prob` to estimate all probabilities conditioned on `y`
#'
#' @param df A `data.frame`/`tibble`
#' @param col A Boolean/logical column
#'
#' @return A table containing the estimated probability.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#'
#' coin <- c('H', 'T')
#' prob <- c(0.55, 0.45)
#'
#' init_trials(1000) %>%
#'    mutate(three.tosses = map(.trial, \(i) sample(coin, 3, replace=TRUE, prob = prob)),
#'           first.head = map_lgl(three.tosses, \(x) pluck(x, 1) == 'H'),
#'           second.tail = map_lgl(three.tosses, \(x) pluck(x, 2) == 'T'),
#'    ) -> df
#'
#' # Or use `View(df)` to see the whole tableau
#' df %>% glimpse
#'
#' df %>%
#'   estimate_all_prob
#'
#' # Package the whole experiment in one pipe
#'
#' (init_trials(1000)
#'    %>% mutate(three.tosses = map(.trial, \(i) sample(coin, 3, replace=TRUE, prob = prob)),
#'               first.head = map_lgl(three.tosses, \(x) pluck(x, 1) == 'H'),
#'               second.tail = map_lgl(three.tosses, \(x) pluck(x, 2) == 'T'),
#'               )
#'    # %>% walk(glimpse) #uncomment to inspect first few rows of tableau
#'    %>% estimate_all_prob
#'  )
#' # Example 2. Estimate P(first is H | at least on T) and Estimate P(second is T | at least on H)
#'
#' (init_trials(1000)
#'    %>% mutate(three.tosses = map(.trial, \(i) sample(coin, 3, replace=TRUE, prob = prob)),
#'               first.head = map_lgl(three.tosses, \(x) pluck(x, 1) == 'H'),
#'               second.tail = map_lgl(three.tosses, \(x) pluck(x, 2) == 'T'),
#'               at.least.one.head = map_lgl(three.tosses, \(x) length(keep(x, \(t) t == 'H')) >= 1)
#'               )
#'    # %>% walk(glimpse) #uncomment to inspect first few rows of tableau
#'    %>% group_by(at.least.one.head)
#'    %>% estimate_all_prob
#'  )
estimate_all_prob<- function(df) {
  df %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.logical), ~ mean(.x, na.rm = TRUE)))
}
