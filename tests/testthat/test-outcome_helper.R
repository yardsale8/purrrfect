test_that("num.successes works", {
  x <- list(c('Red', 'Green', 'Blue'))
  expect_equal(num.successes(x, 'Red'), 1)
  expect_equal(num.successes(x, \(x) x != 'Green'), 2)
})
