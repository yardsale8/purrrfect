test_that("take.one.sample has the correct structure and length", {
  x <- c('H', 'T')
  out <- take.one.sample(x, 4, replace = TRUE)
  # Output is a list of length 1
  expect_type(out, "list")
  expect_type(out[[1]], "character")
  expect_length(out, 1)
  expect_length(out[[1]], 4)
})


test_that("sample without replacement and default size returns whole x", {
  x <- c('H', 'T')
  expect_length(take.one.sample(x), 1)
  expect_length(take.one.sample(x)[[1]], 2)
})

test_that("sample until with replacement and atomic element works", {
  x <- c('H', 'T')
  actual <- sample_until(x, 'H', replace = TRUE)
  expect_true(all(num_successes(x, 'H') == 1))
})

test_that("sample until without replacement and atomic element works", {
  x <- c('H', 'T')
  actual <- sample_until(x, 'H', replace = FALSE)
  expect_true(all(num_successes(actual, 'H') == 1))
  expect_true(all(length(x) >= length(actual)))
})

test_that("sample until with replacement and predicate works", {
  x <- c('B', 'B', 'B', 'W', 'W')
  f <- \(x) num_successes(x, 'B') == 2
  actual <- sample_until(x, f, replace = TRUE)
  expect_true(all(num_successes(actual, 'B') == 2))
})

test_that("sample until without replacement and predicate works", {
  x <- c('B', 'B', 'B', 'W', 'W')
  f <- \(x) num_successes(x, 'B') == 2
  actual <- sample_until(x, f, replace = FALSE)
  expect_true(all(num_successes(actual, 'B') == 2))
  expect_true(all(length(x) >= length(actual)))
})
