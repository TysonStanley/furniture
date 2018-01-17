#' Get Row Means
#' 
#' Does what \code{rowMeans()} does but without having to cbind the variables. Makes it easier to use
#' with the tidyverse
#' 
#' @param ... the values in the variable that are to be replaced by either NA's or the value set by the user. Can be a function (or multiple functions) to specify values to change (e.g., is.nan(), is.na()).
#' @param na.rm default is FALSE
#' 
#' @return the row means
#'
#'
#' @export
rowmeans = function(..., na.rm=FALSE){
  rowMeans(cbind(...))
}


#' Get Row Sums
#' 
#' Does what \code{rowSums()} does but without having to cbind the variables. Makes it easier to use
#' with the tidyverse
#' 
#' @param ... the values in the variable that are to be replaced by either NA's or the value set by the user. Can be a function (or multiple functions) to specify values to change (e.g., is.nan(), is.na()).
#' @param na.rm default is FALSE
#' 
#' @return the row means
#'
#'
#' @export
rowsums = function(..., na.rm=FALSE){
  rowSums(cbind(...))
}
