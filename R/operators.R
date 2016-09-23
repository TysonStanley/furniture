
#' Comparisons Operator
#' 
#' This operator takes one variable and tests if it satisfies multiple conditions. 
#' It is related to \code{\%in\%} but has the additional feature of allowing for >, <, >=, and <= .
#' Use it just as you would \code{\%in\%}. It is named "on" because that is a common verb for
#' furniture (e.g., sit \emph{on} a chair, place it \emph{on} the table, etc.)
#' 
#' @param lhs the left hand side of the operator, a vector
#' @param rhs the right hand side of the operator, a vector of numeric or string values
#' 
#' 
#' @examples 
#' 
#' x = c(1,2,3,4,3,2,1,8,9,7)
#' y = rnorm(10)
#' z = c("Yes", "No", "Yes", "No", "No", "Yes", "No", "No", "Yes", "No")
#' data = data.frame(x, y, z)
#' 
#' data$x %on% c(2, 4, ">5")
#' ifelse(data$x %on% c(2, 4, ">5"), 1, 0)
#' 
#' @export
`%on%` <- function(lhs, rhs){
  match = match.call()
  lhss  = match[[2]]
  rhss  = match[[3]]
  
  .x = eval(lhss)
  .y = eval(rhss)

  if (!is.vector(.y))
    stop("The right hand side needs to be a vector.")
  
  .l1 = list()
  for (i in seq_along(.y)){
    if (grepl(">|<", .y[[i]])){
      .e = parse(text = paste(".x", parse(text =deparse(.y[[i]]))))
      .l1[[i]] = eval(.e)
    } else {
      .l1[[i]] = .x %in% .y[[i]]
    }
  }
  
  .l2 = do.call("cbind", .l1)
  .l  = apply(.l2, 1, any)
  return(.l)
  
}


#' Simple Crosstabs Operator
#' 
#' This operator takes two variables and computes a simple cross tab.
#' 
#' @param lhs the left hand side of the operator, a vector
#' @param rhs the right hand side of the operator, a vector
#' 
#' 
#' @examples 
#' 
#' b = c(1,0,0,1,1,0,1,1,1,0)
#' x = c(1,2,3,2,3,3,1,0,0,0)
#' y = rnorm(10)
#' z = c("Yes", "No", "Yes", "No", "No", "Yes", "No", "No", "Yes", "No")
#' data = data.frame(b, x, y, z)
#' 
#' factor(data$x) %xt% factor(data$b)
#' 
#' @export
`%xt%` <- function(lhs, rhs){
  match = match.call()
  lhss  = match[[2]]
  rhss  = match[[3]]
  
  .x = eval(lhss)
  .y = eval(rhss)
  
  if (!is.factor(.x) | !is.factor(.y))
    stop("Both vectors need to be factors.")
  
  .tab = table("First"=.x, "Second"=.y)
  .ctest = chisq.test(.tab)
  return(list("CrossTab"=.tab, "Test"=.ctest))
  
}





