library(furniture)
library(testthat)

test_that("tableX produces tableX", {
  x  <- runif(100)
  y  <- rnorm(100)
  z  <- factor(sample(c(0,1), 100, replace=TRUE))
  a  <- factor(sample(c(1,2), 100, replace=TRUE))
  b  <- factor(sample(c(1,2,3,4), 100, replace=TRUE))
  df <- data.frame(x, y, z, a, b)
  
  x <- rep(0, times=7)
  d <- rep(0, times=7)
  
  ## Simple
  expect_s3_class(tableX(df, a, b), "table")
  ## d not in df
  expect_error(tableX(df, d))
})

