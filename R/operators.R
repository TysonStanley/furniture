
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
  
  .tab = table("Left"=.x, "Right"=.y)
  .ctest = chisq.test(.tab)
  return(list("CrossTab"=.tab, "Test"=.ctest))
  
}





