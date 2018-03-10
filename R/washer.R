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
#' @return the original vector (although if the original was a factor, it was changed to a character) with the values changed where indicated. 
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
  
  fct = is.factor(x)
  
  for (i in seq_along(c(...))){
    if (!is.function(c(...)[[i]])){
      j = c(...)[i]
      if (sum(x == j, na.rm=TRUE) > 0 & fct){
        x = as.character(x)
      }
    } 
  }
  
  for (i in seq_along(c(...))){
    if (is.function(c(...)[[i]])){
      x[c(...)[[i]](x)] = value
    } else {
      j = c(...)[i]
      x[x == j] = value
    }
  }
  
  if (fct){
    x = as.factor(x)
  }
  
  x
}



