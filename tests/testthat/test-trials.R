test_that("init_trials works", {
  expect_equal(init_trials(5), tidyr::tibble(.trial = 1:5))
})

test_that("add_trials works", {
  out <- tidyr::tribble(~coin, c('H', 'T')) %>% add_trials(10)
  exp <- tidyr::tribble(~coin, ~.trial, c('H', 'T'), 1:10) %>% tidyr::unnest_longer(.trial)
  expect_true(all.equal.list(out, exp))
})
