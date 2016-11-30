library(furniture)
library(testthat)

test_that("tableM produces correct table", {
  x  <- runif(1000)
  x  <- sample(c(x, NA), 1000, replace = TRUE)
  y  <- rnorm(1000)
  z  <- factor(sample(c(0,1,NA), 1000, replace=TRUE))
  a  <- factor(sample(c(1,2,NA), 1000, replace=TRUE))
  b  <- factor(sample(c(1,2,3,4,NA), 1000, replace=TRUE))
  df <- data.frame(x, y, z, a, b)
  
  x <- rep(0, times=7)
  d <- rep(0, times=7)
  
  ## No miss_var
  expect_error(tableM(df, x, y, z, a))
  ## b not in df
  expect_error(tableM(df, d))
  ## Testing miss_var
  expect_s3_class(tableM(df, a, x, y, miss_var=~factor(z)), "table1")
  ## Testing test
  expect_s3_class(tableM(df, a, x, y, miss_var=~factor(z), 
                         test=FALSE), "table1")
  ## Other output types
  expect_s3_class(tableM(df, a, x, y, 
                         miss_var=~factor(z), 
                         test=TRUE, 
                         output_type = "markdown", 
                         align = c("l", "c", "c")), "knitr_kable")
  ## Alternative miss_var
  expect_s3_class(tableM(df, z, x, y, miss_var="a", 
                         test=TRUE), "table1")
  ## Other formats
  expect_s3_class(tableM(df, z, x, y, miss_var=~a, 
                         test=TRUE, 
                         format_output = "full"), "table1")
  expect_s3_class(tableM(df, z, x, y, miss_var=~a, 
                         test=TRUE, 
                         format_output = "stars"), "table1")
  ## Error of quotes
  expect_error(tableM(df, "a", miss_var=~z))
  ## Different miss_var produce same
  expect_equivalent(tableM(df, a, x, y, miss_var=~z), tableM(df, a, x, y, miss_var="z"))
  ## Variable names
  expect_s3_class(tableM(df, z, x, y, miss_var=~a, 
                         test=TRUE, 
                         var_names = c("Z", "X", "Y")), "table1")
  ## miss_var labels
  expect_s3_class(tableM(df, z, x, y, miss_var=~a, 
                         test=TRUE, 
                         miss_var_labels = c("male", "female")), "table1")
  ## NAkeep
  expect_s3_class(tableM(df, z, x, y, miss_var=~a, 
                         test=TRUE, 
                         NAkeep = TRUE), "table1")
  ## Piping
  expect_equal(tableM(df, a, x, y, miss_var=~z, piping=TRUE), df)
  expect_equivalent(tableM(df, a, x, miss_var=~z, 
                           piping = TRUE), 
                    tableM(df, a, x, y, miss_var="z", output_type = "markdown",
                           piping = TRUE))
  ## Test type == or
  expect_s3_class(tableM(df, z, x, y, miss_var=~a, 
                         test=TRUE, test_type = "or"), "table1")
  ## Format number
  expect_s3_class(tableM(df, z, x, y, miss_var=~a, 
                         test=TRUE, format_number = TRUE), "table1")
  ## Taking a character var as the miss_var
  expect_s3_class(tableM(df, z, x, y, miss_var=~as.character(a), 
                         test=TRUE), "table1")
  ## Index
  expect_s3_class(tableM(df, c(1:3), miss_var=~a, 
                         test=TRUE, 
                         NAkeep = TRUE), "table1")
  ## Four Level miss_var
  expect_s3_class(tableM(df, c(1:3), miss_var=~b, 
                         test=TRUE, 
                         NAkeep = TRUE), "table1")
  ## Row Wise
  expect_s3_class(tableM(df, c(1:3), miss_var=~b, 
                          row_wise = TRUE,
                          test=TRUE, 
                          NAkeep = TRUE), "table1")
  ## Text2 output
  expect_s3_class(tableM(df, c(1:3), miss_var=~b, 
                         row_wise = TRUE,
                         test=TRUE, 
                         output_type = "text2"), "table1")
  expect_s3_class(tableM(df, c(1:3), miss_var=~b, 
                         test=TRUE, 
                         output_type = "text2"), "table1")
})

