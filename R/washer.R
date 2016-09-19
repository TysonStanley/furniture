#' Wash Your Data
#' 
#' Washes the data by replacing values with either NA's or other values set by the user. 
#' Useful for replacing values such as 777's or 999's that represent missing values in survey research.
#' Can also perform many useful functions on factors (e.g., removing a level, replacing a level, etc.)
#' 
#' @param x	 the variable to have values adjusted
#' @param ... the values in the variable that are to be replaced by either NA's or the value set by the user. Can be a function (or multiple functions) to specify values to change (e.g., is.nan(), is.na()).
#' @param value (optional) if specified, the values in ... will be replaced by this value (must be a single value)
#' 
#' @return A table with the number of observations, means/frequencies and standard deviations/percentages is returned. The object is a \code{table1} class object with a print method. Can be printed in \code{LaTex} form.
#'
#' @examples 
#' x = c(1:20, NA, NaN)
#' washer(x, 9, 10)
#' washer(x, 9, 10, value=0)
#' washer(x, 1:10)
#' washer(x, is.na, is.nan, value=0)
#' washer(x, is.na, is.nan, 1:3, value=0)
#'
#' @export
washer = function(x, ..., value=NA){
  if (is.factor(x)){
    x = as.character(x)
  }
  for (i in seq_along(c(...))){
    if (is.function(c(...)[[i]])){
      x[c(...)[[i]](x)] = value
    } else {
      j = c(...)[i]
      x[x == j] = value
    }
  }
  return(x)
}



