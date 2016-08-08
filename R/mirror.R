
mirror = function(data, variable, lower, upper, replace=FALSE){
  .rc = data.frame(".Original"=NA, ".Recode"=NA)
  for(i in 0:(upper-lower)){
    .rc[i+1, 1:2] = c(i+1, upper - i)
  }
  data = merge(data, .rc, by.x=variable, by.y=".Original")
  if (replace){
    data[, which(names(data) == variable)] = data[dim(data)[2]]
    data[dim(data)[2]] = NULL
  }
  return(data)
}


