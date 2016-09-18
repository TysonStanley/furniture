tp = function(formula, data, bin.link = "logit", count.family = poisson(link = "log")){
  ## Formula and Data ##
  f   = formula
  y   = f[[2]]
  data$y01 = ifelse(data[, grepl(y, names(data))] > 0, 1, 0)
  x = paste(f)[-c(1)]
  x = x[2]
  dataC = data[y > 0, ]
  
  ## Binary ##
  f01 = as.formula(paste("y01 ~", x))
  fitBin   = glm(f01, data=data, family=binomial(link = bin.link))
  
  ## Count ##
  fitCount = glm(as.formula(f), data=dataC, family=count.family)
  
  ## Variables in Model ##
  vars = names(fitCount$model)[-1]
  
  ## Final Object ##
  tp1 = list("Binary"=fitBin, "Count"=fitCount, "vars"=vars)
  class(tp1) = "tp"
  return(tp1)
}


print.tp = function(x, ...){
  cat("---\nBinary Portion of Two-Part Model\n\n")
  print(summary.glm(x$Binary)$coef, ...)
  cat("\n---\nCount Portion of Two-Part Model\n\n")
  print(summary.glm(x$Count)$coef, ...)
  cat("---\nModel 1: binomial with", x$Binary$family[[2]], "link",
      "\nModel 2:", x$Count$family[[1]], "with", x$Count$family[[2]], "link \n")
}

summary.tp = function(object, ...){
  cat("--- \n Binary Portion \n")
  print(summary.glm(object$Binary), ...)
  cat("--- \n Count Portion \n")
  print(summary.glm(object$Count), ...)
}


tp2frames = function(model, bootsize=100, ci=.95){
  
  stopifnot(class(model)[[1]]=="tp")
  aveMarg = list()
  
  for (p in c("Binary", "Count")){
    ## Initial Model and Data
    data   = model[[p]]$data
    family = model[[p]]$family
    m = model[[p]]
    
    ## Derivatives
    pdf  = ifelse(family[[2]]=="probit",
                  mean(dnorm(predict(m, type = "link"))),
                  ifelse(family[[2]]=="logit", 
                         mean(dlogis(predict(m, type = "link"))),
                         ifelse(family[[2]]=="log",
                                mean(predict(m, type = "resp")),
                                ifelse(family[[2]]=="identity", 1, NA))))
    ## Average Marginal Effects
    aveMarg[[p]] = pdf*coef(m)
  }
  
  for (p in c("Binary", "Count")){
    data   = model[[p]]$data
    family = model[[p]]$family
    m = model[[p]]
    ## Bootstrap CI's
    n = dim(data)[1]
    boot.samples = matrix(sample(1:n, size=n*bootsize, replace=TRUE), n, bootsize)
    lmed = function(x){
      fit = glm(m$formula, data = data[x, ], family = family)
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
    if (p == "Binary"){
      marg1 = boots
    } else {
      marg2 = boots
    }
  }
  boots = marg1 * marg2
  low = apply(boots, 1, FUN=function(x) quantile(x, 1-ci, na.rm=TRUE))
  hi  = apply(boots, 1, FUN=function(x) quantile(x, ci, na.rm=TRUE))
  final = data.frame("AME"=aveMarg$Binary + aveMarg$Count, 
                     "Lower"=low, 
                     "Upper"=hi)
  ame = list("AME"   = final,
             "Model" = summary(model),
             "Variables" = row.names(final),
             "Family" = family,
             "Bootsize" = bootsize,
             "Alpha" = 1 - ci,
             "Data" = data)
  class(ame) = c("frames", "list")
  return(ame)
}

