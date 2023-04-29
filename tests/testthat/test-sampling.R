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
  expect_length(take.one.sample(x), 1)
  expect_length(take.one.sample(x)[[1]], 2)
})
