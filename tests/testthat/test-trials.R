test_that("init_trials works", {
  expect_equal(init_trials(5), tidyr::tibble(.trial = 1:5))
})

test_that("add_trials works with no groups", {
  out <- tidyr::tribble(~coin, c('H', 'T')) %>% add_trials(10, .group = FALSE)
  exp <- tidyr::tribble(~coin, ~.trial, c('H', 'T'), 1:10) %>% tidyr::unnest_longer(.trial)
  expect_true(all.equal.list(out, exp))
})

test_that("add_trials works with groups", {
  out <- (tidyr::tribble(~coin, c('H', 'T'))
          %>% add_trials(10, .group = TRUE)
          %>% dplyr::summarise(m = mean(.trial))
         )
  exp <- (tidyr::tribble(~coin, ~.trial, c('H', 'T'), 1:10)
          %>% tidyr::unnest_longer(.trial)
          %>% dplyr::group_by(dplyr::pick(-.trial))
          %>% dplyr::summarise(m = mean(.trial))
          )
  expect_true(all.equal.list(out, exp))
})
