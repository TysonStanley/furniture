#' @title Long to Wide Data Reshaping
#' @author Tyson S. Barrett
#' 
#' @description \code{wide()} is a wrapper of \code{reshape()} that takes the data 
#' from a long format to a wide format. All arguments that \code{reshape()} accepts,
#' other than \code{direction} since it is set to "wide" here, are accepted 
#' here as well.
#' 
#' @param data the data.frame containing the wide format data
#' @param v.names the variable names in quotes of the measures to be separated into multiple columns based on the time variable
#' @param ... other arguments accepted by \code{reshape()}
#' 
#' @seealso \code{stats::reshape()}
#' 
#' @importFrom stats reshape
#' 
#' @export
wide <- function(data, v.names, ...){
  UseMethod("wide", data)
}

#' @importFrom stats reshape
#' @export
wide.tibble <- function(data, v.names, ...){
  data = as.data.frame(data)
  newd = stats::reshape(data, v.names, ...,
                        direction = "wide")
  return(newd)
}

#' @importFrom stats reshape
#' @export
wide.tbl <- function(data, v.names, ...){
  data = as.data.frame(data)
  newd = stats::reshape(data, v.names, ...,
                        direction = "wide")
  return(newd)
}

#' @importFrom stats reshape
#' @export
wide.data.frame <- function(data, v.names, ...){
  newd = stats::reshape(data, v.names, ...,
                        direction = "wide")
  return(newd)
}

#' @importFrom stats reshape
#' @export
wide.matrix <- function(data, v.names, ...){
  newd = stats::reshape(data, v.names, ...,
                        direction = "wide")
  return(newd)
}

#' @title Wide to Long Data Reshaping
#' @author Tyson S. Barrett
#' 
#' @description \code{long()} is a wrapper of \code{reshape()} that takes the data 
#' from a wide format to a long format. All arguments that \code{reshape()} accepts,
#' other than \code{direction} since it is set to "long" here, are accepted 
#' here as well.
#' 
#' @param data the data.frame containing the wide format data
#' @param varying the variables that are time-varying that are to be placed in long format, 
#' needs to be in the format \code{list(c("x1", "x2"), c("z1", "z2"), etc.)}
#' @param ... other arguments accepted by \code{reshape()}
#' 
#' @seealso \code{stats::reshape()} and \code{sjmisc::to_long()}
#'
#' @importFrom stats reshape
#' 
#' @export

long <- function(data, varying, ...){
  UseMethod("long", data)
}

#' @importFrom stats reshape
#' @export
long.tibble <- function(data, varying, ...){
  data = as.data.frame(data)
  newd = stats::reshape(data, varying, ...,
                        direction = "long")
  return(newd)
}

#' @importFrom stats reshape
#' @export
long.tbl <- function(data, varying, ...){
  data = as.data.frame(data)
  newd = stats::reshape(data, varying, ...,
                        direction = "long")
  return(newd)
}

#' @importFrom stats reshape
#' @export
long.data.frame <- function(data, varying, ...){
  newd = stats::reshape(data, varying, ...,
                        direction = "long")
  return(newd)
}

#' @importFrom stats reshape
#' @export
long.matrix <- function(data, varying, ...){
  newd = stats::reshape(data, varying, ...,
                        direction = "long")
  return(newd)
}

