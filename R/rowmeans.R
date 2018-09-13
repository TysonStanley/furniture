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

#' Get Row Means within a Pipe
#' 
#' Does what \code{rowMeans()} does can be used one its own without dplyr::mutate() 
#' within a pipe.
#' 
#' @param data the dataframe that contains the variables to get the row means from
#' @param new_var the name of the new variable for which you'll put the row means in quotes
#' @param ... the variables (unquoted) to be included in the row means
#' @param na.rm should the missing values be ignored? default is FALSE
#' 
#' @return the row means included within the data.frame
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
#'   mutate_rowmeans("y_z_mean", y, z))
#' data2 <- data %>%
#'   mutate_rowmeans("y_z_mean", y, z, na.rm=TRUE))
#' 
#' }
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @export
mutate_rowmeans = function(data, new_var, ..., na.rm=FALSE){
  data %>%
    dplyr::select(...) %>%
    dplyr::mutate(!!new_var := rowMeans(., na.rm = na.rm))
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


#' Get Row Sums within a Pipe
#' 
#' Does what \code{rowSums()} does can be used one its own without dplyr::mutate() 
#' within a pipe.
#' 
#' @param data the dataframe that contains the variables to get the row means from
#' @param new_var the name of the new variable for which you'll put the row means in quotes
#' @param ... the variables (unquoted) to be included in the row means
#' @param na.rm should the missing values be ignored? default is FALSE
#' 
#' @return the row means included within the data.frame
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
#'   mutate_rowsums("y_z_sums", y, z))
#' data2 <- data %>%
#'   mutate_rowsums("y_z_sums", y, z, na.rm=TRUE))
#' 
#' }
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @export
mutate_rowsums = function(data, new_var, ..., na.rm=FALSE){
  data %>%
    dplyr::select(...) %>%
    dplyr::mutate(!!new_var := rowSums(., na.rm = na.rm))
}


