library(furniture)
library(testthat)

test_that("simple_table1 produces table1", {
  x  <- runif(1000)
  y  <- rnorm(1000)
  z  <- factor(sample(c(0,1), 1000, replace=TRUE))
  a  <- factor(sample(c(1,2), 1000, replace=TRUE))
  b  <- factor(sample(c(1,2,3,4), 1000, replace=TRUE))
  df <- data.frame(x, y, z, a, b)
  
  x <- rep(0, times=7)
  d <- rep(0, times=7)
  
  ## Simple
  expect_s3_class(simple_table1(df, x, y, z, factor(a)), "table1")
  ## b not in df
  expect_error(simple_table1(df, d))
  ## Testing splitby
  expect_s3_class(simple_table1(df, a, x, y, splitby=~factor(z)), "table1")
  ## Testing test
  expect_s3_class(simple_table1(df, a, x, y, splitby=~factor(z), 
                                test=TRUE), "table1")
  ## Other output types
  expect_s3_class(simple_table1(df, a, x, y, 
                                 splitby=~factor(z), 
                                 test=TRUE, 
                                 output_type = "markdown", 
                                 align = c("l", "c", "c")), "knitr_kable")
  ## Alternative splitby
  expect_s3_class(simple_table1(df, z, x, y, splitby="a", 
                                test=TRUE), "table1")
  ## OTher formats
  expect_s3_class(simple_table1(df, z, x, y, splitby=~a, 
                                 test=TRUE, 
                                 format_output = "full"), "table1")
  expect_s3_class(simple_table1(df, z, x, y, splitby=~a, 
                                 test=TRUE, 
                                 format_output = "stars"), "table1")
  ## Error of quotes
  expect_error(simple_table1(df, "a", splitby=~z))
  ## Different splitby produce same
  expect_equivalent(simple_table1(df, a, x, y, splitby=~z), simple_table1(df, a, x, y, splitby="z"))
  ## Variable names
  expect_s3_class(simple_table1(df, z, x, y, splitby=~a, 
                               test=TRUE, 
                               var_names = c("Z", "X", "Y")), "table1")
  ## splitby labels
  expect_s3_class(simple_table1(df, z, x, y, splitby=~a, 
                               test=TRUE, 
                               splitby_labels = c("male", "female")), "table1")
  ## NAkeep
  expect_s3_class(simple_table1(df, z, x, y, splitby=~a, 
                               test=TRUE, 
                               NAkeep = TRUE), "table1")
  ## Piping
  expect_equal(simple_table1(df, a, x, y, splitby=~z, piping=TRUE), df)
  expect_equivalent(simple_table1(df, a, x, splitby=~z, 
                                  piping = TRUE), 
                    simple_table1(df, a, x, y, splitby="z", output_type = "markdown",
                                  piping = TRUE))
  ## Format Number
  expect_s3_class(simple_table1(df, z, x, y, splitby=~a, 
                                test=TRUE, format_number = TRUE), "table1")
  ## Taking a character var as the splitby
  expect_s3_class(simple_table1(df, z, x, y, splitby=~as.character(a), 
                                test=TRUE), "table1")
  ## Index
  expect_s3_class(simple_table1(df, c(1:3), splitby=~a, 
                                 test=TRUE, 
                                 NAkeep = TRUE), "table1")
  ## Four Level Splitby
  expect_s3_class(simple_table1(df, c(1:3), splitby=~b, 
                                 test=TRUE, 
                                 NAkeep = TRUE), "table1")
  ## Row Wise
  expect_s3_class(simple_table1(df, c(1:3), splitby=~b, 
                                row_wise = TRUE,
                                test=TRUE, 
                                NAkeep = TRUE), "table1")
})

