#' Get Row Means
#' 
#' Does what \code{rowMeans()} does but without having to cbind the variables. Makes it easier to use
#' with the tidyverse
#' 
#' @param ... the variables (unquoted) to be included in the row means
#' @param na.rm should the missing values be ignored? default is FALSE
#' 
#' @return the row means
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(furniture)
#' library(tidyverse)
#' 
#' data <- data.frame(
#'   x = sample(c(1,2,3,4), 100, replace=TRUE),
#'   y = rnorm(100),
#'   z = rnorm(100)
#' )
#' 
#' data2 <- data %>%
#'   mutate(y_z_mean = rowmeans(y, z))
#' data2 <- data %>%
#'   mutate(y_z_mean = rowmeans(y, z, na.rm=TRUE))
#' 
#' }
#'
#' @export
rowmeans = function(..., na.rm=FALSE){
  rowMeans(cbind(...), na.rm = na.rm)
}

#' Get Row Means With N Missing Values Per Row
#' 
#' Does what \code{furniture::rowmeans()} does while allowing a certain number (\code{n}) to have missing values. 
#' 
#' @param ... the variables (unquoted) to be included in the row means
#' @param n the number of values without missingness required to get the row mean
#' 
#' @return the row means
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(furniture)
#' library(dplyr)
#' 
#' data <- data.frame(
#'   x = sample(c(1,2,3,4), 100, replace=TRUE),
#'   y = rnorm(100),
#'   z = rnorm(100)
#' )
#' 
#' data2 <- mutate(data, x_y_z_mean = rowmeans.n(x, y, z, n = 2))
#' 
#' }
#'
#' @export
rowmeans.n <- function(..., n){
  ifelse(rowmeans(is.na(cbind(...)) <= n),
         rowmeans(..., na.rm = TRUE),
         NA)
}

#' Get Row Sums
#' 
#' Does what \code{rowSums()} does but without having to cbind the variables. Makes it easier to use
#' with the tidyverse
#' 
#' @param ... the variables to be included in the row sums
#' @param na.rm should the missing values be ignored? default is FALSE
#' 
#' @return the row sums
#'
#'
#' @examples 
#' 
#' \dontrun{
#' 
#' library(furniture)
#' library(tidyverse)
#' 
#' data <- data.frame(
#'   x = sample(c(1,2,3,4), 100, replace=TRUE),
#'   y = rnorm(100),
#'   z = rnorm(100)
#' )
#' 
#' data2 <- data %>%
#'   mutate(y_z_sum = rowsums(y, z))
#' data2 <- data %>%
#'   mutate(y_z_sum = rowsums(y, z, na.rm=TRUE))
#' 
#' }
#' 
#' 
#' @export
rowsums = function(..., na.rm=FALSE){
  rowSums(cbind(...), na.rm = na.rm)
}

#' Get Row Sums With N Missing Values Per Row
#' 
#' Does what \code{furniture::rowsums()} does while allowing a certain number (\code{n}) to have missing values. 
#' 
#' @param ... the variables (unquoted) to be included in the row means
#' @param n the number of values without missingness required to get the row mean
#' 
#' @return the row sums
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(furniture)
#' library(dplyr)
#' 
#' data <- data.frame(
#'   x = sample(c(1,2,3,4), 100, replace=TRUE),
#'   y = rnorm(100),
#'   z = rnorm(100)
#' )
#' 
#' data2 <- mutate(data, x_y_z_mean = rowsums.n(x, y, z, n = 2))
#' 
#' }
#'
#' @export
rowsums.n <- function(..., n){
  ifelse(rowsums(is.na(cbind(...)) <= n),
         rowsums(..., na.rm = TRUE),
         NA)
}



