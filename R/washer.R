
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


