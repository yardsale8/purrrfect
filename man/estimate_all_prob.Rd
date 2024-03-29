% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.R
\name{estimate_all_prob}
\alias{estimate_all_prob}
\title{Estimate all probabilities}
\usage{
estimate_all_prob(df)
}
\arguments{
\item{df}{A \code{data.frame}/\code{tibble}}

\item{col}{A Boolean/logical column}
}
\value{
A table containing the estimated probability.
}
\description{
Estimate probabilities for all Boolean/logical vectors in \code{df} by using
\itemize{
\item \code{df \%>\% estimate_all_prob} to estimate all marginal probabilities
\item \code{df \%>\% group_by(y) \%>\% estimate_all_prob} to estimate all probabilities conditioned on \code{y}
}
}
\examples{
library(dplyr)
library(purrr)

coin <- c('H', 'T')
prob <- c(0.55, 0.45)

init_trials(1000) \%>\%
   mutate(three.tosses = map(.trial, \(i) sample(coin, 3, replace=TRUE, prob = prob)),
          first.head = map_lgl(three.tosses, \(x) pluck(x, 1) == 'H'),
          second.tail = map_lgl(three.tosses, \(x) pluck(x, 2) == 'T'),
   ) -> df

# Or use `View(df)` to see the whole tableau
df \%>\% glimpse

df \%>\%
  estimate_all_prob

# Package the whole experiment in one pipe

(init_trials(1000)
   \%>\% mutate(three.tosses = map(.trial, \(i) sample(coin, 3, replace=TRUE, prob = prob)),
              first.head = map_lgl(three.tosses, \(x) pluck(x, 1) == 'H'),
              second.tail = map_lgl(three.tosses, \(x) pluck(x, 2) == 'T'),
              )
   # \%>\% walk(glimpse) #uncomment to inspect first few rows of tableau
   \%>\% estimate_all_prob
 )
# Example 2. Estimate P(first is H | at least on T) and Estimate P(second is T | at least on H)

(init_trials(1000)
   \%>\% mutate(three.tosses = map(.trial, \(i) sample(coin, 3, replace=TRUE, prob = prob)),
              first.head = map_lgl(three.tosses, \(x) pluck(x, 1) == 'H'),
              second.tail = map_lgl(three.tosses, \(x) pluck(x, 2) == 'T'),
              at.least.one.head = map_lgl(three.tosses, \(x) length(keep(x, \(t) t == 'H')) >= 1)
              )
   # \%>\% walk(glimpse) #uncomment to inspect first few rows of tableau
   \%>\% group_by(at.least.one.head)
   \%>\% estimate_all_prob
 )
}
