library(furniture)
library(testthat)

test_that("table1 produces table1", {
  x  <- runif(1000)
  y  <- rnorm(1000)
  z  <- factor(sample(c(0,1), 1000, replace=TRUE))
  a  <- factor(sample(c(1,2), 1000, replace=TRUE))
  df <- data.frame(x, y, z, a)
  
  x <- rep(0, times=7)
  b <- rep(0, times=7)
  
  expect_s3_class(table1(df, x, y, z, factor(a)), "table1")
  
  expect_error(table1(df, b))
  
  expect_s3_class(table1(df, a, x, y, splitby=~factor(z)), "table1")
  
  expect_s3_class(table1(df, a, x, y, splitby=~factor(z), 
                         test=TRUE), "table1")
  
  expect_s3_class(table1(df, a, x, y, 
                         splitby=~factor(z), 
                         test=TRUE, 
                         output_type = "markdown", 
                         align = c("l", "c", "c")), "knitr_kable")
  
  expect_s3_class(table1(df, z, x, y, splitby="a", 
                         test=TRUE), "table1")
  
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         format_output = "full"), "table1")
  
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         format_output = "stars"), "table1")
  
  expect_error(table1(df, "a", splitby=~z))
  
  expect_equivalent(table1(df, a, x, y, splitby=~z), table1(df, a, x, y, splitby="z"))
  
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         var_names = c("Z", "X", "Y")), "table1")
  
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         splitby_labels = c("male", "female")), "table1")
  
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         NAkeep = TRUE), "table1")
  
  expect_equal(table1(df, a, x, y, splitby=~z, piping=TRUE), df)
  
  expect_equivalent(table1(df, a, x, splitby=~z, 
                           piping = TRUE), 
                    table1(df, a, x, y, splitby="z", output_type = "markdown",
                           piping = TRUE))
})

