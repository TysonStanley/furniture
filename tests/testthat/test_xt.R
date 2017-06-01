library(furniture)
library(testthat)

test_that("xt produces correct output", {
  expect_error(c(1,2,3,2,3,1,3) %xt% factor(c(1,0,1,0,1,1,1)))
  expect_equal(length(factor(c(1,2,1,2,2,1,2,1)) %xt% factor(c(1,0,1,0,1,1,1,0))), 2)
})

