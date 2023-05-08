test_case_default <- function(functor, f) {
  base <- tidyr::tibble(.trial = sapply(1:5, list))
  actual <- base %>% functor(f, onto = .trial)
  expect <- tidyr::tibble(.trial = sapply(1:5, list),
                   .outcome = sapply(1:5, f),
                  )
  all.equal.list(actual, expect)
}

test_case_new_name <- function(functor, f) {
  base <- tidyr::tibble(.trial = sapply(1:5, list))
  actual <- base %>% functor(f, onto = .trial, as=out)
  expect <- tidyr::tibble(.trial = sapply(1:5, list),
                   out = sapply(1:5, f),
                  )
  all.equal.list(actual, expect)
}

test_that("col_map works with default as=.outcome", {
  f <- \(i) seq(1, i, 1)
  expect_true(test_case_default(col_map, f))
  expect_true(test_case_new_name(col_map, f))
})


test_that("col_map_lgl works with default as=.outcome", {
  f <- \(i) i > 2
  expect_true(test_case_default(col_map_lgl, f))
  expect_true(test_case_new_name(col_map_lgl, f))
})

test_that("col_map_int works with default as=.outcome", {
  f <- \(i) i + 2
  expect_true(test_case_default(col_map_int, f))
  expect_true(test_case_new_name(col_map_int, f))
})

test_that("col_map_int works with default as=.outcome", {
  f <- \(i) i + 2.0
  expect_true(test_case_default( col_map_dbl, f))
  expect_true(test_case_new_name(col_map_dbl, f))
})

test_that("col_map_chr works with default as=.outcome", {
  expect_true(test_case_default( col_map_chr, paste0))
  expect_true(test_case_new_name(col_map_chr, paste0))
})

test_that("col_map_vec works with default as=.outcome", {
  f <- \(i) as.Date("01-01-01")
  base <- tidyr::tibble(.trial = sapply(1:5, list))
  actual <- base %>% col_map_vec(f, onto = .trial)
  expect <-
    base %>%
    dplyr::mutate(.outcome = purrr::map_vec(.trial, f))
  expect_true(all.equal.list(actual, expect))
})
