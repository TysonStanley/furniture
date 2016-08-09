
washer = function(x, ..., value=NA){
  if (is.factor(x)){
    x = as.character(x)
  }
  if (is.function(list(...)[[1]])){
    .f = function(x) list(...)[[1]]
    x[list(...)[[1]](x)] = value
  } else {
    for (i in c(...)){
      x[x == i] = value
    }
  }
  return(x)
}

