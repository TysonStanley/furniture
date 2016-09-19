
frames = function(model, bootsize=100, ci=.95){
  
  ## Initial Model and Data
  data   = model$data
  family = model$family
  if(family[[2]] == "inverse"){
    stop(message("Must be probit, logit, log, or idenitity linked"))
  }
  
  ## Derivatives
  pdf  = ifelse(family[[2]]=="probit",
                mean(dnorm(predict(model, type = "link"))),
                ifelse(family[[2]]=="logit", 
                       mean(dlogis(predict(model, type = "link"))),
                       ifelse(family[[2]]=="log",
                              mean(predict(model, type = "resp")),
                              ifelse(family[[2]]=="identity", 1, NA))))
  ## Average Marginal Effects
  aveMarg = pdf*coef(model)
  
  ## Bootstrap CI's
  n = dim(data)[1]
  boot.samples = matrix(sample(1:n, size=n*bootsize, replace=TRUE), n, bootsize)
  lmed = function(x){
    fit = glm(model$formula, data = data[x,], family = model$family)
    return(fit)
  }
  pdfed = function(x){
    pdf  = ifelse(family[[2]]=="probit",
                  mean(dnorm(predict(x, type = "link"))),
                  ifelse(family[[2]]=="logit", 
                         mean(dlogis(predict(x, type = "link"))),
                         ifelse(family[[2]]=="log",
                                mean(predict(x, type = "resp")),
                                ifelse(family[[2]]=="identity", 1, NA))))
    return(pdf)
  }
  boot.coefs = apply(boot.samples, 2, lmed)
  boot.Margs = lapply(boot.coefs, function(x) pdfed(x))
  ## Average Marginal Effects
  d = list()
  for (i in 1:length(boot.Margs)){
    bootMarg = boot.Margs[[i]] * lapply(boot.coefs, coef)[[i]]
    d[[i]] = data.frame(bootMarg)
  }
  boots = do.call("cbind", d)
  low = apply(boots, 1, FUN=function(x) quantile(x, 1-ci, na.rm=TRUE))
  hi  = apply(boots, 1, FUN=function(x) quantile(x, ci, na.rm=TRUE))
  .final = data.frame("AME"   = aveMarg, 
                      "Lower" = low, 
                      "Upper" = hi)
  .ame = list("AME"   = .final,
             "Model" = summary(model),
             "Variables" = row.names(.final),
             "Family" = family,
             "Bootsize" = bootsize,
             "Alpha" = 1 - ci,
             "Data" = data)
  class(.ame) = c("frames", "list")
  return(.ame)
}

print.frames = function(x, ...){
  cat("--- \n Average Marginal Effects \n")
  print(x$AME, ...)
}

summary.frames = function(object, ...){
  cat("---\n Model \n")
  print(object$Model, ...)
  cat("---\n Average Marginal Effects \n\n")
  print(object$AME, ...)
}
