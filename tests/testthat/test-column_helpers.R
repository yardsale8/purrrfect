test_that("col_num_successes with default name works", {
  base <- tidyr::tibble(n = 1:5,
                        out = sapply(1:5, \(n) rep_len('H', n)),
                        )
  actual <- base %>% col_num_successes(out, 'H')
  expect <- tidyr::tibble(n = 1:5,
                          out = sapply(1:5, \(n) rep_len('H', n)),
                          .successes = 1:5
                          )
  expect_true(all.equal.list(actual, expect))
})

test_that("col_num_successes with user defined name works", {
  base <- tidyr::tibble(n = 1:5,
                        out = sapply(1:5, \(n) rep_len('H', n)),
                        )
  actual <- base %>% col_num_successes(out, 'H', name = s)
  expect <- tidyr::tibble(n = 1:5,
                          out = sapply(1:5, \(n) rep_len('H', n)),
                          s = 1:5
                          )
  expect_true(all.equal.list(actual, expect))
})
