library(furniture)
library(testthat)

test_that("washer produces correct output vectors", {
  expect_equal(washer(c(1,2,3,4), 1), c(NA,2,3,4))
  expect_equal(washer(c(1,2,3,4), 1, value=0), c(0,2,3,4))
  expect_equal(washer(as.character(c(1,2,3,4)), 1, value=0), as.character(c(0,2,3,4)))
})

