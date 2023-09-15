test_that("parameters with one parameter works", {
  xs <- seq(5,15,5)
  actual = parameters(~x, xs)
  expect  = tibble::tribble(~x, xs) %>% tidyr::unnest_longer(x)
  expect_true(all.equal.list(actual, expect))
})

test_that("parameters with two parameter works", {
  xs <- seq(5,15,5)
  ys <- seq(1, 4, 1)
  actual = parameters(~x, ~y, xs, ys)
  expect  =  tibble::tribble(~x, ~y, xs, ys) %>% tidyr::unnest_longer(x) %>% tidyr::unnest_longer(y)
  expect_true(all.equal.list(actual, expect))
})
