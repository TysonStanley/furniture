
washer = function(x, ..., value=NULL){
  if (is.null(value)){
    for (i in c(...)){
      x[x == i] = NA
    }
  } else {
    for (i in c(...)){
      x[x == i] = value
    }
  }
  return(x)
}

