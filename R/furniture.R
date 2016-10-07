
#' furniture
#' 
#' The furniture package offers simple functions and an operator that are aimed at helping
#' applied researchers explore and communicate their data as well as clean their data
#' in a tidy way. The package follows similar semantics to the "tidyverse" packages. It contains 
#' two main tools (along with two operators):
#' \itemize{
#'   \item \code{table1} provides a well-formatted descriptive table often seen 
#'         as table 1 in academic journals,
#'   \item \code{washer} provides a simple way to clean up data where there are placeholder
#'         values, and
#'   \item \code{\%xt\%} is an operator that takes two factor variables and creates a cross tabulation and
#'         tests for significance via a chi-square test.
#' }
#' 
#' It works similar to operators in packages like \code{magrittr}.
#' 
#' Table 1 is the main function in furniture. It is important in both data exploration 
#' and data communication. With minimal cleaning, the outputted table can be put into 
#' an academic, peer reviewed journal manuscript.
#' 
#' 
#' 
#' @examples
#' \dontrun{
#' 
#' library(furniture)
#' 
#' ## Table 1
#' data %>%
#'   table1(var1, var2, var3, 
#'          splitby = ~groupvar,
#'          test = TRUE)
#' 
#' ## Washer
#' x = washer(x, 7, 8, 9)
#' x = washer(x, is.na, value=0)
#' 
#' ## Crosstabs Operator
#' f1 %xt% f2
#' 
#' }
#' 
#' @docType package
#' @name furniture
NULL