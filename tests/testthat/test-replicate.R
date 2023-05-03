get_out <- function(func, x, n = 10) func(n, x)
get_exp <- function(x, n=10) {
  tidyr::tribble(~.trial, ~.outcome, 1:10, x) %>%
    tidyr::unnest_longer(.trial)
}
all_equal_replicate <- function(func, x) {
  all.equal.list(get_out(func, x), get_exp(x))
}

test_that("replicate works", {
  expect_true(all_equal_replicate(replicate, c('H', 'T')))
})

test_that("replicate_lgl works", {
  expect_true(all_equal_replicate(replicate_lgl, TRUE))
})

test_that("replicate_int works", {
  expect_true(all_equal_replicate(replicate_int, 1))
})

test_that("replicate_dbl works", {
  expect_true(all_equal_replicate(replicate_dbl, 2.0))
})

test_that("replicate_chr works", {
  expect_true(all_equal_replicate(replicate_chr, "a"))
})

test_that("replicate_vec works", {
  expect_true(all_equal_replicate(replicate_vec, as.Date("01-01-01")))
})
