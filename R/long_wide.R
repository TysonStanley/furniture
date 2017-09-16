#' @title Long to Wide Data Reshaping
#' @author Tyson S. Barrett
#' 
#' @description \code{wide()} is a wrapper of \code{stats::reshape()} that takes the data 
#' from a long format to a wide format. 
#' 
#' @param data the data.frame containing the wide format data
#' @param v.names the variable names in quotes of the measures to be separated into multiple columns based on the time variable
#' @param timevar the variable name in quotes of the time variable
#' @param id the ID variable name in quotes
#' 
#' @seealso \code{stats::reshape()}, \code{tidyr::spread()}
#' 
#' @importFrom stats reshape
#' 
#' @export
wide <- function(data, v.names, timevar, id=NULL){
  UseMethod("wide", data)
}

#' @importFrom stats reshape
#' @export
wide.tibble <- function(data, v.names=NULL, timevar, id=NULL){
  data = as.data.frame(data)
  if (any(grepl("[i|I][d|D]", names(data))) & is.null(id)){
    id = names(data)[grep("[i|I][d|D]", names(data))[1]]
    message(paste("id =", id))
  }
  newd = stats::reshape(data, v.names, timevar, idvar = id, varying = NULL, 
                        direction = "wide")
  return(newd)
}

#' @importFrom stats reshape
#' @export
wide.tbl_df <- function(data, v.names=NULL, timevar, id=NULL){
  data = as.data.frame(data)
  if (any(grepl("[i|I][d|D]", names(data))) & is.null(id)){
    id = names(data)[grep("[i|I][d|D]", names(data))[1]]
    message(paste("id =", id))
  }
  newd = stats::reshape(data, v.names, timevar, idvar = id, varying = NULL,
                        direction = "wide")
  return(newd)
}

#' @importFrom stats reshape
#' @export
wide.data.frame <- function(data, v.names=NULL, timevar, id=NULL){
  if (any(grepl("[i|I][d|D]", names(data))) & is.null(id)){
    id = names(data)[grep("[i|I][d|D]", names(data))[1]]
    message(paste("id =", id))
  }
  newd = stats::reshape(data, v.names, timevar, idvar = id, varying = NULL,
                        direction = "wide")
  return(newd)
}

#' @importFrom stats reshape
#' @export
wide.matrix <- function(data, v.names=NULL, timevar, id=NULL){
  if (any(grepl("[i|I][d|D]", names(data))) & is.null(id)){
    id = names(data)[grep("[i|I][d|D]", names(data))[1]]
    message(paste("id =", id))
  }
  newd = stats::reshape(data, v.names, timevar, idvar = id, varying = NULL,
                        direction = "wide")
  return(newd)
}

#' @title Wide to Long Data Reshaping
#' @author Tyson S. Barrett
#' 
#' @description \code{long()} is a wrapper of \code{stats::reshape()} that takes the data 
#' from a wide format to a long format. It can also handle unbalanced data (where some measures
#' have different number of "time points").
#' 
#' @param data the data.frame containing the wide format data
#' @param ... the variables that are time-varying that are to be placed in long format, 
#' needs to be in the format \code{c("x1", "x2"), c("z1", "z2"), etc.}. If the data is 
#' unbalanced (e.g., there are three time points measured for one variable but only two for another),
#' using the placeholder variable \code{miss}, helps fix this.
#' @param v.names a vector of the names for the newly created variables (length same as number of vectors in \code{varying})
#' @param id the ID variable in quotes
#' @param timevar the column with the "time" labels
#' @param times the labels of the \code{timevar} (default is numeric)
#' @param sep the separating character between the wide format variable names (default is \code{""}); e.g. "x1" and "x2" would create the variable name of "x"; only applicable if \code{v.names}
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
#' ldf1 <- long(df, 
#'              c("x1", "x2"), c("y1", "y2"),
#'              v.names = c("x", "y"))
#' 
#' ## "Unbalanced" Data
#' ldf2 = long(df, 
#'             c("x1", "x2", "x3"), c("y1", "y2", "miss"),
#'             v.names = c("x", "y"))
#' 
#' 
#' @export

long <- function(data, ..., v.names=NULL, id=NULL, timevar=NULL, times=NULL, sep=""){
  UseMethod("long", data)
}

#' @importFrom stats reshape
#' @export
long.tibble <- function(data, ..., v.names=NULL, id=NULL, timevar=NULL, times=NULL, sep=""){
  varying = list(...)
  if (is.null(id)){
    if (any(grepl("[i|I][d|D]", names(data)))){
      id = names(data)[grep("[i|I][d|D]", names(data))[1]]
      message(paste("id =", id))
    } else {
      id = "id"
    }
  }
  if (is.null(timevar)){
    timevar = "time"
  }
  if (is.null(times)){
    times = seq_along(varying[[1]])
  }
  data = as.data.frame(data)
  data$miss = NA
  ids = 1:NROW(data)
  newd = stats::reshape(data, varying, v.names, timevar = timevar,
                        times = times, idvar = id, ids = ids, sep=sep,
                        direction = "long")
  if (any(names(newd) == "miss")){
    var_loc = which(names(newd) == "miss")
    newd = newd[, -var_loc]
  }
  return(newd)
}

#' @importFrom stats reshape
#' @export
long.tbl_df <- function(data, ..., v.names=NULL, id=NULL, timevar=NULL, times=NULL, sep=""){
  varying = list(...)
  if (is.null(id)){
    if (any(grepl("[i|I][d|D]", names(data)))){
      id = names(data)[grep("[i|I][d|D]", names(data))[1]]
      message(paste("id =", id))
    } else {
      id = "id"
    }
  }
  if (is.null(timevar)){
    timevar = "time"
  }
  if (is.null(times)){
    times = seq_along(varying[[1]])
  }
  data = as.data.frame(data)
  data$miss = NA
  ids = 1:NROW(data)
  newd = stats::reshape(data, varying, v.names, timevar = timevar,
                        times = times, idvar = id, ids = ids, sep=sep,
                        direction = "long")
  if (any(names(newd) == "miss")){
    var_loc = which(names(newd) == "miss")
    newd = newd[, -var_loc]
  }
  return(newd)
}

#' @importFrom stats reshape
#' @export
long.data.frame <- function(data, ..., v.names=NULL, id=NULL, timevar=NULL, times=NULL, sep=""){
  varying = list(...)
  if (is.null(id)){
    if (any(grepl("[i|I][d|D]", names(data)))){
      id = names(data)[grep("[i|I][d|D]", names(data))[1]]
      message(paste("id =", id))
    } else {
      id = "id"
    }
  }
  if (is.null(timevar)){
    timevar = "time"
  }
  if (is.null(times)){
    times = seq_along(varying[[1]])
  }
  data$miss = NA
  ids = 1:NROW(data)
  newd = stats::reshape(data, varying, v.names, timevar = timevar,
                        times = times, idvar = id, ids = ids, sep=sep,
                        direction = "long")
  if (any(names(newd) == "miss")){
    var_loc = which(names(newd) == "miss")
    newd = newd[, -var_loc]
  }
  return(newd)
}

#' @importFrom stats reshape
#' @export
long.matrix <- function(data, ..., v.names=NULL, id=NULL, timevar=NULL, times=NULL, sep=""){
  varying = list(...)
  if (is.null(id)){
    if (any(grepl("[i|I][d|D]", names(data)))){
      id = names(data)[grep("[i|I][d|D]", names(data))[1]]
      message(paste("id =", id))
    } else {
      id = "id"
    }
  }
  if (is.null(timevar)){
    timevar = "time"
  }
  if (is.null(times)){
    times = seq_along(varying[[1]])
  }
  data$miss = NA
  ids = 1:NROW(data)
  newd = stats::reshape(data, varying, v.names, timevar = timevar,
                        times = times, idvar = id, ids = ids, sep=sep,
                        direction = "long")
  if (any(names(newd) == "miss")){
    var_loc = which(names(newd) == "miss")
    newd = newd[, -var_loc]
  }
  return(newd)
}

