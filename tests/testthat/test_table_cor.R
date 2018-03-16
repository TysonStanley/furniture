library(furniture)
library(testthat)

test_that("table1 produces table1", {
  x  <- runif(1000)
  y  <- rnorm(1000)
  z  <- rnorm(1000)
  a  <- rnorm(1000)
  b  <- rnorm(1000)
  df <- data.frame(x, y, z, a, b)
  
  x <- rep(0, times=7)
  d <- rep(0, times=7)
  
  ## Simple
  expect_s3_class(tableC(df, x, y, z, a, b), "table1")
  ## d not in df
  expect_error(tableC(df, d))
  expect_s3_class(tableC(df, a, x, y, rounding = 4), "table1")
  expect_s3_class(tableC(df, a, x, y, cor_type = "spearman"), "table1")
  expect_s3_class(tableC(df, a, x, y, na.rm = TRUE), "table1")
  ## Other output types
  expect_s3_class(tableC(df, a, x, y,
                         output = "markdown"), "knitr_kable")
  expect_s3_class(tableC(df, a, x, y,
                         output = "latex2"), "latex2")
})


