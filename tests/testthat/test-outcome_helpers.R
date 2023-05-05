test_that("Atomic objects work", {
  outcomes <- list(c('H', 'H', 'T', 'H', 'T'))
  expect_equal(num_successes(outcomes, 'H'), 3)
  all_tails <- list(rep_len('T', 5))
  expect_equal(num_successes(all_tails, 'H'), 0)
  expect_equal(num_successes(all_tails, 'T'), 5)
})

test_that("predicate functions work", {
  outcomes <- list(c('H', 'H', 'T', 'H', 'T'))
  expect_equal(num_successes(outcomes, \(x) x != 'H'), 2)
  all_tails <- list(rep_len('T', 5))
  expect_equal(num_successes(all_tails, \(x) x == 'H'), 0)
  expect_equal(num_successes(all_tails, \(x) x != 'H'), 5)
})

test_that("Works on empty vectors", {
  empty <- list(c())
  expect_equal(num_successes(empty, 'H'), 0)
  expect_equal(num_successes(empty, \(x) x != 'H'), 0)
})

