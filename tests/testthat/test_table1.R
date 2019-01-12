library(testthat)

test_that("table1 produces table1", {
  x  <- runif(1000)
  y  <- rnorm(1000)
  z  <- factor(sample(c(0,1), 1000, replace=TRUE))
  a  <- factor(sample(c(1,2), 1000, replace=TRUE))
  b  <- factor(sample(c(1,2,3,4), 1000, replace=TRUE))
  df <- data.frame(x, y, z, a, b)
  
  x <- rep(0, times=7)
  d <- rep(0, times=7)
  
  ## Simple
  expect_s3_class(table1(df, x, y, z, factor(a)), "table1")
  ## b not in df
  expect_error(table1(df, d))
  ## Testing splitby
  expect_s3_class(table1(df, a, x, y, splitby=~factor(z)), "table1")
  ## Testing test
  expect_s3_class(table1(df, a, x, y, splitby=~factor(z), 
                         test=TRUE), "table1")
  ## Other output types
  expect_s3_class(table1(df, a, x, y, 
                         splitby=~factor(z), 
                         test=TRUE, 
                         output = "markdown", 
                         align = c("l", "c", "c")), "knitr_kable")
  ## Alternative splitby
  expect_s3_class(table1(df, z, x, y, splitby="a", 
                         test=TRUE), "table1")
  ## Other formats
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         type = "full"), "table1")
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         type = "stars"), "table1")
  ## Error of quotes
  expect_error(table1(df, "a", splitby=~z))
  ## Rounding_perc
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         type = "stars",
                         rounding_perc=2), "table1")
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         type = c("stars", "simple"),
                         rounding_perc=2), "table1")
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         type = c("stars", "condense"),
                         rounding_perc=2), "table1")
  ## Different splitby produce same
  expect_equivalent(table1(df, a, x, y, splitby=~z), table1(df, a, x, y, splitby="z"))
  ## Variable names
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         var_names = c("Z", "X", "Y")), "table1")
  ## splitby labels
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         splitby_labels = c("male", "female")), "table1")
  ## na.rm
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, 
                         na.rm = FALSE), "table1")
  ## Format number
  expect_s3_class(table1(df, z, x, y, splitby=~a, 
                         test=TRUE, format_number = TRUE), "table1")
  ## Taking a character var as the splitby
  expect_s3_class(table1(df, z, x, y, splitby=~as.character(a), 
                         test=FALSE), "table1")
  ## Index
  expect_s3_class(table1(df, c(1:3), splitby=~a, 
                         test=TRUE, 
                         na.rm = FALSE), "table1")
  ## Four Level Splitby
  expect_s3_class(table1(df, c(1:3), splitby=~b, 
                         test=TRUE, 
                         na.rm = FALSE), "table1")
  ## Row Wise
  expect_s3_class(table1(df, c(1:3), splitby=~b, 
                          row_wise = TRUE,
                          test=TRUE, 
                          na.rm = FALSE), "table1")
  ## Text2 output
  expect_s3_class(table1(df, c(1:3), splitby=~b, 
                         row_wise = TRUE,
                         test=TRUE, 
                         output = "text2"), "table1")
  expect_s3_class(table1(df, c(1:3), splitby=~b, 
                         test=TRUE, 
                         output = "text2"), "table1")
  expect_s3_class(df %>% table1(c(1:3), splitby=~b, 
                         row_wise = TRUE,
                         test=TRUE, 
                         output = "text2"), "table1")
  expect_s3_class(df %>% table1(c(1:3), splitby=~b, 
                         test=TRUE, 
                         output = "text2"), "table1")
  expect_s3_class(df %>% table1(c(1:3), splitby=~b, 
                         row_wise = TRUE,
                         test=TRUE, 
                         output = "latex"), "knitr_kable")
  expect_s3_class(table1(df, c(1:3), splitby=~b, 
                         test=TRUE, 
                         output = "html"), "knitr_kable")
  ## Simple Format
  expect_s3_class(table1(df, c(1:3), splitby=~b, 
                         test=TRUE, 
                         output = "text2"), "table1")
  ## All option
  expect_s3_class(table1(df, splitby=~b, 
                         test=TRUE, 
                         output = "text2"), "table1")
  ## Medians, Condensed, Simple
  expect_s3_class(table1(df, splitby=~b, 
                         test=TRUE, 
                         output = "text2",
                         type = "simple",
                         second = c("a")), "table1")
  expect_s3_class(table1(df, splitby=~b, 
                         test=FALSE, 
                         output = "text2",
                         type = c("simple", "condensed", "pvalues"),
                         second = c("a", "c")), "table1")
  ## Export
  # expect_s3_class(table1(df, splitby=~b, 
  #                        test=FALSE, 
  #                        output = "text2",
  #                        export = "test_tab",
  #                        second = c("a", "c")), "table1")
  ## latex2 output
  expect_is(table1(df, splitby=~b, 
                         test=FALSE, 
                         output = "latex2"), "latex2")
  expect_is(table1(df, splitby=~b, 
                       test=TRUE, 
                       output = "latex2"), "latex2")
  ## header_labels
  expect_s3_class(table1(df, splitby=~b, 
                         test=FALSE, 
                         output = "text2",
                         header_labels = c(" ", "P-Val"),
                         second = c("a", "c")), "table1")
  ## Single Variable
  expect_s3_class(table1(df, x,
                         splitby=~b, 
                         test=FALSE, 
                         output = "text2",
                         header_labels = c(" ", "P-Val"),
                         second = c("a", "c")), "table1")
  expect_s3_class(df %>%
                    group_by(b) %>%
                    table1(x,
                         test=FALSE, 
                         output = "text2",
                         header_labels = c(" ", "P-Val"),
                         second = c("a", "c")), "table1")
  ## Total
  expect_s3_class(df %>%
                    group_by(b) %>%
                    table1(x,
                           test=FALSE, 
                           output = "text2",
                           header_labels = c(" ", "P-Val"),
                           second = c("a", "c"),
                           total = TRUE), "table1")
  expect_s3_class(df %>%
                    group_by(b) %>%
                    table1(x,
                           test=TRUE, 
                           output = "text2",
                           header_labels = c(" ", "P-Val"),
                           second = c("a", "c"),
                           total = TRUE), "table1")
  expect_s3_class(df %>%
                    group_by(b) %>%
                    table1(x,
                           test=FALSE, 
                           output = "text2",
                           header_labels = c(" ", "P-Val"),
                           total = TRUE), "table1")
  expect_s3_class(df %>%
                    group_by(b) %>%
                    table1(x,
                           test=FALSE, 
                           second = c("a", "c"),
                           total = TRUE), "table1")
  
  ## Variables with no variability
  expect_warning(df %>%
                   mutate(x = 1) %>%
                   group_by(b) %>%
                   table1(x,
                          test=FALSE, 
                          output = "text2",
                          header_labels = c(" ", "P-Val"),
                          second = c("a", "c")))
  expect_failure(expect_warning(df %>%
                                  table1(x,
                                         test=FALSE, 
                                         output = "text2",
                                         header_labels = c(" ", "P-Val"),
                                         second = c("a", "c"))))
  
})

