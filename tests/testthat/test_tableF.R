library(furniture)
library(testthat)

test_that("tableF produces tableF", {
  x  <- runif(1000)
  y  <- rnorm(1000)
  z  <- factor(sample(c(0,1), 1000, replace=TRUE))
  a  <- factor(sample(c(1,2), 1000, replace=TRUE))
  b  <- factor(sample(c(1,2,3,4), 1000, replace=TRUE))
  c  <- sample(c(rnorm(10), NA), 1000, replace=TRUE)
  df <- data.frame(x, y, z, a, b, c)
  
  x <- rep(0, times=7)
  d <- rep(0, times=7)
  
  ## no missing
  expect_s3_class(tableF(df, x), "tableF")
  ## missing
  expect_s3_class(tableF(df, c), "tableF")
  ## Testing splitby
  expect_s3_class(tableF(df, a, splitby=z), "tableF")
})

