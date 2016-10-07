library(furniture)
library(testthat)

test_that("table1 produces table1", {
  x  <- c(1,2,NA,2,NA,3,NA)
  y  <- rnorm(7)
  z  <- factor(c(0,1,1,0,0,0,1))
  df <- data.frame(x, y, z, "a"=c(1,2,1,2,1,2,1))
  
  x <- rep(0, times=7)
  b <- rep(0, times=7)
  
  expect_s3_class(table1(df, x, y, z, factor(a)), "table1")
  expect_error(table1(df, b))
})

