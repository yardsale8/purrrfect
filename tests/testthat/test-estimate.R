test_that("one-way tabulate works", {
  df <- tidyr::tibble(v = c(rep_len('S', 6), rep_len('F', 4)))
  actual <- df %>% tabulate(v)
  expect <- tidyr::tribble(~"X = F", ~"X = S", 0.4, 0.6)
  expect_true(all.equal.list(actual, expect))
})

test_that("two-way tabulate works", {
  df <- tidyr::tibble(x = c(rep_len('S', 6), rep_len('F', 4),
                            rep_len('S', 7), rep_len('F', 3)
                            ),
                      y = c(rep_len('A', 10), rep_len('B', 10))
                      )
  actual <- df %>% dplyr::group_by(y) %>% tabulate(x)
  expect <- tidyr::tribble(~"y", ~"X = F", ~"X = S",
                           "A", 0.4, 0.6,
                           "B", 0.3, 0.7,
                           )
  expect_true(all.equal.list(actual, expect))
})

test_that("one-way estmate_prob works", {
  df <- tidyr::tibble(v = c(rep_len(TRUE, 6), rep_len(FALSE, 4)))
  actual <- df %>% estimate_prob(v)
  expect <- tidyr::tribble(~"v", 0.6)
  expect_true(all.equal.list(actual, expect))
})

test_that("two-way estimate_prob works", {
  df <- tidyr::tibble(x = c(rep_len(TRUE, 6), rep_len(FALSE, 4),
                            rep_len(TRUE, 7), rep_len(FALSE, 3)
  ),
  y = c(rep_len('A', 10), rep_len('B', 10))
  )
  actual <- df %>% dplyr::group_by(y) %>% estimate_prob(x)
  expect <- tidyr::tribble(~"y", ~"x",
                           "A", 0.6,
                           "B", 0.7,
  )
  expect_true(all.equal.list(actual, expect))
})

test_that("one-way estmate_all_prob works", {
  df <- tidyr::tibble(u = c(rep_len(TRUE, 7), rep_len(FALSE, 3)),
                      v = c(rep_len(TRUE, 6), rep_len(FALSE, 4)),
                      )
  actual <- df %>% estimate_all_prob
  expect <- tidyr::tribble(~"u", ~"v", 0.7, 0.6, )
  expect_true(all.equal.list(actual, expect))
})

test_that("two-way estimate_all_prob works", {
  df <- tidyr::tibble(x1 = c(rep_len(TRUE, 6), rep_len(FALSE, 4),
                            rep_len(TRUE, 7), rep_len(FALSE, 3)),
                      x2 = c(rep_len(TRUE, 5), rep_len(FALSE, 5),
                            rep_len(TRUE, 5), rep_len(FALSE, 5)),
                      y = c(rep_len('A', 10), rep_len('B', 10))
                     )
  actual <- df %>% dplyr::group_by(y) %>% estimate_all_prob
  expect <- tidyr::tribble(~"y", ~"x1", ~"x2",
                           "A", 0.6, 0.5,
                           "B", 0.7, 0.5,
  )
  expect_true(all.equal.list(actual, expect))
})
