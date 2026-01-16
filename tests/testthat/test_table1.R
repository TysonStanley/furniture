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
  expect_identical(
    capture.output(
     df %>%
       group_by(b) %>%
       table1(x,
              test=FALSE, 
              total = TRUE,
              output = "latex2"))[1], 
     c("\\begin{table}[ ht ] "))
  expect_identical(
    capture.output(
      df %>%
        table1(x,
               test=FALSE, 
               total = TRUE,
               output = "latex2"))[1], 
    c("\\begin{table}[ ht ] "))
  
  ## Variables with no variability
  expect_warning(df %>%
                   mutate(x = 1) %>%
                   group_by(b) %>%
                   table1(x,
                          output = "text2",
                          header_labels = c(" ", "P-Val"),
                          second = c("a", "c"),
                          test = TRUE))
  expect_failure(expect_warning(df %>%
                                  table1(x,
                                         test=FALSE,
                                         output = "text2",
                                         header_labels = c(" ", "P-Val"),
                                         second = c("a", "c"))))

})

test_that("NAkeep parameter shows deprecation warning", {
  x  <- runif(100)
  y  <- rnorm(100)
  z  <- factor(sample(c(0,1), 100, replace=TRUE))
  df <- data.frame(x, y, z)

  # Using NAkeep should trigger a deprecation warning
  expect_warning(
    table1(df, x, y, z, NAkeep = TRUE),
    "NAkeep.*deprecated",
    ignore.case = TRUE
  )

  # The warning should mention na.rm as the replacement
  expect_warning(
    table1(df, x, y, z, NAkeep = FALSE),
    "na.rm"
  )

  # Not using NAkeep should not trigger warning
  expect_no_warning(
    table1(df, x, y, z, na.rm = TRUE)
  )

})

test_that("table1 works with empty ... parameter", {
  x  <- runif(100)
  y  <- rnorm(100)
  z  <- factor(sample(c(0,1), 100, replace=TRUE))
  a  <- factor(sample(c(1,2), 100, replace=TRUE))
  df <- data.frame(x, y, z, a)

  # Empty ... with no grouping (should include all variables)
  expect_s3_class(table1(df), "table1")

  # Empty ... with splitby
  expect_s3_class(table1(df, splitby = ~a), "table1")

  # Empty ... with group_by (this was the bug we fixed)
  expect_s3_class(df %>% group_by(a) %>% table1(), "table1")

  # Empty ... with group_by and tests
  expect_s3_class(df %>% group_by(a) %>% table1(test = TRUE), "table1")

  # Empty ... with multiple grouping variables
  expect_s3_class(df %>% group_by(a, z) %>% table1(), "table1")

  # Verify that grouping variable is not duplicated in output
  tab <- df %>% group_by(a) %>% table1()
  tab_df <- as.data.frame(tab)
  # The grouping variable 'a' should not appear as a row in the table
  # (it should only be the stratifying variable)
  expect_false(any(grepl("^a$", tab_df[[1]])))

  # Empty ... with total column
  expect_s3_class(df %>% group_by(a) %>% table1(total = TRUE), "table1")
})

test_that("table1_gt produces gt_tbl", {
  skip_if_not_installed("gt")

  x  <- runif(100)
  y  <- rnorm(100)
  z  <- factor(sample(c(0,1), 100, replace=TRUE))
  a  <- factor(sample(c(1,2), 100, replace=TRUE))
  df <- data.frame(x, y, z, a)

  # Basic table1_gt
  tab1 <- df %>%
    group_by(a) %>%
    table1(x, y, z)

  expect_s3_class(table1_gt(tab1), "gt_tbl")

  # With spanner
  expect_s3_class(table1_gt(tab1, spanner = "Group A"), "gt_tbl")

  # Without grouping
  tab2 <- df %>%
    table1(x, y, z)

  expect_s3_class(table1_gt(tab2), "gt_tbl")

  # With tests
  tab3 <- df %>%
    group_by(a) %>%
    table1(x, y, z, test = TRUE)

  expect_s3_class(table1_gt(tab3), "gt_tbl")
})

test_that("table1_flextable produces flextable", {
  skip_if_not_installed("flextable")

  x  <- runif(100)
  y  <- rnorm(100)
  z  <- factor(sample(c(0,1), 100, replace=TRUE))
  a  <- factor(sample(c(1,2), 100, replace=TRUE))
  df <- data.frame(x, y, z, a)

  # Basic table1_flextable
  tab1 <- df %>%
    group_by(a) %>%
    table1(x, y, z)

  expect_s3_class(table1_flextable(tab1), "flextable")

  # With spanner
  expect_s3_class(table1_flextable(tab1, spanner = "Group A"), "flextable")

  # Without grouping
  tab2 <- df %>%
    table1(x, y, z)

  expect_s3_class(table1_flextable(tab2), "flextable")

  # With tests
  tab3 <- df %>%
    group_by(a) %>%
    table1(x, y, z, test = TRUE)

  expect_s3_class(table1_flextable(tab3), "flextable")
})



