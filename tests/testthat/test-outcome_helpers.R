test_that("atomic objects work", {
  outcomes <- list(c('h', 'h', 't', 'h', 't'))
  expect_equal(num_successes_int(outcomes, 'h'), 3)
  expect_equal(purrr::map_int(outcomes, \(x) num_successes(x, 'h')), 3)
  all_tails <- list(rep_len('t', 5))
  expect_equal(num_successes_int(all_tails, 'h'), 0)
  expect_equal(num_successes_int(all_tails, 't'), 5)
  expect_equal(purrr::map_int(all_tails, \(x) num_successes(x, 'h')), 0)
  expect_equal(purrr::map_int(all_tails, \(x) num_successes(x, 't')), 5)
})

test_that("predicate functions work", {
  outcomes <- list(c('H', 'H', 'T', 'H', 'T'))
  expect_equal(num_successes_int(outcomes, \(x) x != 'H'), 2)
  expect_equal(purrr::map_int(outcomes, \(x) num_successes(x, \(t) t != 'H')), 2)
  all_tails <- list(rep_len('T', 5))
  expect_equal(num_successes_int(all_tails, \(x) x == 'H'), 0)
  expect_equal(num_successes_int(all_tails, \(x) x != 'H'), 5)
  expect_equal(purrr::map_int(all_tails, \(x) num_successes(x, \(t) t == 'H')), 0)
  expect_equal(purrr::map_int(all_tails, \(x) num_successes(x, \(t) t != 'H')), 5)
})

test_that("Works on empty vectors", {
  empty <- list(c())
  expect_equal(num_successes_int(empty, 'H'), 0)
  expect_equal(num_successes_int(empty, \(x) x != 'H'), 0)
})

