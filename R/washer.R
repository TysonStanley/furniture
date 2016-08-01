
washer = function(x, ..., value=NA){
  for (i in c(...)){
    x[x == i] = value
  }
  return(x)
}

