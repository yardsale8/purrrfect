test_that("is.equal works", {
  expect_equal(is.equal(2, seq(1,10)),
               `==`(2, seq(1,10)))
})

test_that("not.equal works", {
  expect_equal(not.equal(2, seq(1,10)),
               `!=`(2, seq(1,10)))
})

test_that("strictly.greater works", {
  expect_equal(strictly.greater(2, seq(1,10)),
               `>`(2, seq(1,10)))
})

test_that("strictly.less works", {
  expect_equal(strictly.less(2, seq(1,10)),
               `<`(2, seq(1,10)))
})

test_that("at.least works", {
  expect_equal(at.least(2, seq(1,10)),
               `>=`(2, seq(1,10)))
})

test_that("at.most works", {
  expect_equal(at.most(2, seq(1,10)),
               `<=`(2, seq(1,10)))
})
