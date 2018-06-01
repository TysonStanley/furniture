library(furniture)
library(testthat)
library(dplyr)

test_that("long and wide", {
  x1 <- runif(1000)
  x2 <- runif(1000)
  x3 <- runif(1000)
  y1 <- rnorm(1000)
  y2 <- rnorm(1000)
  z  <- factor(sample(c(0,1), 1000, replace=TRUE))
  a  <- factor(sample(c(1,2), 1000, replace=TRUE))
  b  <- factor(sample(c(1,2,3,4), 1000, replace=TRUE))
  df  <- data.frame(x1, x2, x3, y1, y2, z, a, b)
  ldf <- long(df, c("x1", "x2", "x3"), c("y1", "y2", "miss"),
              v.names = c("x", "y"))
  
  ## Long
  expect_s3_class(long(df, 
                       c("x1", "x2"), c("y1", "y2"),
                       sep = ""), "data.frame")
  expect_s3_class(long(df, 
                       c("x1", "x2"), c("y1", "miss"),
                       v.names = c("x", "y"),
                       sep = ""), "data.frame")
  expect_s3_class(long(df, 
                       c("x1", "x2", "x3"), c("y1", "y2", "miss"),
                       v.names = c("x", "y")), "data.frame")
  ## Long Other Classes
  expect_s3_class(long(dplyr::as.tbl(df), 
                       c("x1", "x2"), c("y1", "y2"),
                       sep = ""), "data.frame")
  expect_s3_class(long(dplyr::as.tbl(df), 
                       c("x1", "x2"), c("y1", "miss"),
                       v.names = c("x", "y"),
                       sep = ""), "data.frame")
  expect_s3_class(long(dplyr::as.tbl(df), 
                       c("x1", "x2", "x3"), c("y1", "y2", "miss"),
                       v.names = c("x", "y")), "data.frame")
  ## Wide
  expect_s3_class(wide(ldf, 
                       v.names = c("x", "y"),
                       timevar = "time"), "data.frame")
  expect_s3_class(wide(dplyr::as.tbl(ldf), 
                       v.names = c("x", "y"),
                       timevar = "time"), "data.frame")
  
})

