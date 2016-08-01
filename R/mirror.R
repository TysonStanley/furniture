
mirror = function(data, variable, lower, upper, replace=FALSE){
  recode = data.frame("Original"=NA, "Recode"=NA)
  for(i in 0:(upper-lower)){
    recode[i+1, 1:2] = c(i+1, upper - i)
  }
  data = merge(data, recode, by.x=variable, by.y="Original")
  if (replace){
    data[, which(names(data) == variable)] = data[dim(data)[2]]
    data[dim(data)[2]] = NULL
  }
  return(data)
}


