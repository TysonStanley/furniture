#' Get Row Means
#' 
#' Does what \code{rowMeans()} does but without having to cbind the variables. Makes it easier to use
#' with the tidyverse
#' 
#' @param ... the variables to be included in the row means
#' @param na.rm default is FALSE
#' 
#' @return the row means
#'
#'
#' @export
rowmeans = function(..., na.rm=FALSE){
  rowMeans(cbind(...), na.rm = na.rm)
}


#' Get Row Sums
#' 
#' Does what \code{rowSums()} does but without having to cbind the variables. Makes it easier to use
#' with the tidyverse
#' 
#' @param ... the variables to be included in the row sums
#' @param na.rm default is FALSE
#' 
#' @return the row sums
#'
#'
#' @export
rowsums = function(..., na.rm=FALSE){
  rowSums(cbind(...), na.rm = na.rm)
}
