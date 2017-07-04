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
#' @param timevar the variable name in quotes of the time variable
#' @param idvar the ID variable name in quotes
#' @param ... other arguments accepted by \code{reshape()}
#' 
#' @seealso \code{stats::reshape()}
#' 
#' @importFrom stats reshape
#' 
#' @export
wide <- function(data, v.names, timevar, idvar=NULL, ...){
  UseMethod("wide", data)
}

#' @importFrom stats reshape
#' @export
wide.tibble <- function(data, v.names, timevar, idvar=NULL, ...){
  data = as.data.frame(data)
  if (any(grepl("[i|I][d|D]", names(data)))){
    idvar = names(data)[grep("[i|I][d|D]", names(data))[1]]
    message(paste("idvar =", idvar))
  }
  newd = stats::reshape(data, v.names, timevar, idvar, varying = NULL, ...,
                        direction = "wide")
  return(newd)
}

#' @importFrom stats reshape
#' @export
wide.tbl_df <- function(data, v.names, timevar, idvar=NULL, ...){
  data = as.data.frame(data)
  if (any(grepl("[i|I][d|D]", names(data)))){
    idvar = names(data)[grep("[i|I][d|D]", names(data))[1]]
    message(paste("idvar =", idvar))
  }
  newd = stats::reshape(data, v.names, timevar, idvar, varying = NULL, ...,
                        direction = "wide")
  return(newd)
}

#' @importFrom stats reshape
#' @export
wide.data.frame <- function(data, v.names, timevar, idvar=NULL, ...){
  if (any(grepl("[i|I][d|D]", names(data)))){
    idvar = names(data)[grep("[i|I][d|D]", names(data))[1]]
    message(paste("idvar =", idvar))
  }
  newd = stats::reshape(data, v.names, timevar, idvar, varying = NULL, ...,
                        direction = "wide")
  return(newd)
}

#' @importFrom stats reshape
#' @export
wide.matrix <- function(data, v.names, timevar, idvar=NULL, ...){
  if (any(grepl("[i|I][d|D]", names(data)))){
    idvar = names(data)[grep("[i|I][d|D]", names(data))[1]]
    message(paste("idvar =", idvar))
  }
  newd = stats::reshape(data, v.names, timevar, idvar, varying = NULL, ...,
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
#' needs to be in the format \code{list(c("x1", "x2"), c("z1", "z2"), etc.)}. If the data is 
#' unbalanced (e.g., there are three time points measured for one variable but only two for another),
#' using the placeholder variable \code{miss}, helps fix this.
#' @param ... other arguments accepted by \code{reshape()}
#' 
#' @seealso \code{stats::reshape()} and \code{sjmisc::to_long()}
#'
#' @importFrom stats reshape
#' 
#' @examples 
#' 
#' x1 <- runif(1000)
#' x2 <- runif(1000)
#' x3 <- runif(1000)
#' y1 <- rnorm(1000)
#' y2 <- rnorm(1000)
#' z  <- factor(sample(c(0,1), 1000, replace=TRUE))
#' a  <- factor(sample(c(1,2), 1000, replace=TRUE))
#' b  <- factor(sample(c(1,2,3,4), 1000, replace=TRUE))
#' df  <- data.frame(x1, x2, x3, y1, y2, z, a, b)
#' 
#' ## "Balanced" Data
#' ldf1 <- long(df, varying = list(c("x1", "x2"),
#'                                 c("y1", "y2")),
#'              v.names = c("x", "y"))
#' 
#' ## "Unbalanced" Data
#' ldf2 = long(df, 
#'             varying = list(c("x1", "x2", "x3"),
#'                            c("y1", "y2", "miss")),
#'             v.names = c("x", "y"))
#' 
#' 
#' @export

long <- function(data, varying, ...){
  UseMethod("long", data)
}

#' @importFrom stats reshape
#' @export
long.tibble <- function(data, varying, ...){
  data = as.data.frame(data)
  data$miss = NA
  newd = stats::reshape(data, varying, ...,
                        direction = "long")
  if (any(names(newd) == "miss")){
    var_loc = which(names(newd) == "miss")
    newd = newd[, -var_loc]
  }
  return(newd)
}

#' @importFrom stats reshape
#' @export
long.tbl_df <- function(data, varying, ...){
  data = as.data.frame(data)
  data$miss = NA
  newd = stats::reshape(data, varying, ...,
                        direction = "long")
  if (any(names(newd) == "miss")){
    var_loc = which(names(newd) == "miss")
    newd = newd[, -var_loc]
  }
  return(newd)
}

#' @importFrom stats reshape
#' @export
long.data.frame <- function(data, varying, ...){
  data$miss = NA
  newd = stats::reshape(data, varying, ...,
                        direction = "long")
  if (any(names(newd) == "miss")){
    var_loc = which(names(newd) == "miss")
    newd = newd[, -var_loc]
  }
  return(newd)
}

#' @importFrom stats reshape
#' @export
long.matrix <- function(data, varying, ...){
  data$miss = NA
  newd = stats::reshape(data, varying, ...,
                        direction = "long")
  if (any(names(newd) == "miss")){
    var_loc = which(names(newd) == "miss")
    newd = newd[, -var_loc]
  }
  return(newd)
}

