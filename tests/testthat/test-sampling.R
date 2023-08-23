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

test_that("col_sample with n=N and replace=FALSE returns a permutation", {
  (tidyr::tribble(~coin,
           c('H', 'T'),
  )
  %>% add_trials(10)
  %>% col_sample(coin, 2)
  %>% col_num_successes(.outcome, 'H', name = num_heads)
  %>% col_num_successes(.outcome, 'T', name = num_tails)
  %>% dplyr::mutate(one_of_each = num_heads == 1 & num_tails == 1)
  ) -> output_df
  expect_true(all(output_df$one_of_each))
})



test_that("col_sample with n=5 and replace=True returns the correct(ish) output", {
  (tidyr::tribble(~coin,
           c('H', 'T'),
  )
  %>% add_trials(10)
  %>% col_sample(coin, 5, replace = TRUE)
  %>% col_num_successes(.outcome, 'H', name = num_heads)
  %>% col_num_successes(.outcome, 'T', name = num_tails)
  %>% dplyr::mutate(total_is_five = (num_heads + num_tails) == 5)
  ) -> output_df
  expect_true(all(output_df$total_is_five))
})

test_that("col_sample with n=5, replace=FALSE, and prob = c(1, 0) returns all heads", {
  (tidyr::tribble(~coin,
           c('H', 'T'),
  )
  %>% add_trials(10)
  %>% col_sample(coin, 5, replace = TRUE, prob = c(1, 0))
  %>% col_num_successes(.outcome, 'H', name = num_heads)
  %>% col_num_successes(.outcome, 'T', name = num_tails)
  %>% dplyr::mutate(all_heads = num_heads == 5)
  ) -> output_df
  expect_true(all(output_df$all_heads))
})
