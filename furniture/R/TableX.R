
tableX = function(models, model.names, type="default", level=.95){
  # Title: Table X 
  # Author: Tyson Barrett
  # Purpose: to report important statistics from many linear models in a succint, well-formatted table
  # Arguments: 
  #   1. models is a list of "lm" or "glm" objects
  #   2. model.names is a vector of names for the models (must match length(models))
  #   3. type has two options: "default" which prints the estimates and standard errors
  #                            "exp" which prints the exponentiated estimates and the exponentiated CI's
  #   4. level is the CI level if type == "exp"
  
  ## Error Management ##
  if (type!="default" & type!="exp") 
    stop(cat(paste("The", type, "method is not supported. Yet. Let me know you want it @ t.barrett@aggiemail.usu.edu.",
                   "\n","Types Supported: default (reports estimates and standard errors) 
                   and exp (reports exponentiated estimates and confidence intervals.")))
  if (!all(sapply(models, class) == "lm" | sapply(models, class) == "glm")) 
    stop(cat(paste("The models must be 'lm' or 'glm' objects. If there is another modeling type 
                   let me know you want it @ t.barrett@aggiemail.usu.edu.")))
  
  ## Initial Model Summaries ##
  m = m2 = rows = pval = se = obs = N = list()
  for (i in seq_along(models)){
    summed    = summary(models[[i]])$coefficients
    m[[i]]    = data.frame("Est" = summed[,1])
    se[[i]]   = data.frame("SE"       = summed[,2])
    pval[[i]] = data.frame("PValue"   = summed[,4]) 
    obs[[i]]  = data.frame("Est" = sum(!is.na(models[[i]]$fitted.values)))
  }
  
  ## Specific Types
  if (type=="default"){
    for (i in seq_along(m)){
      m2[[i]] =
        data.frame(
          "Est"   = round(m[[i]], 2), 
          "SE"    = round(se[[i]], 2),
          "Sig"   = paste(ifelse(pval[[i]] < .001, "***",
                                 ifelse(pval[[i]] < .01, "**",
                                        ifelse(pval[[i]] < .05, "*", "")))),
          "names" = rownames(m[[i]]))
      N[[i]] = data.frame("Est"=obs[[i]], "SE"=NA, "Sig"=NA, "names" = "N")
      m2[[i]] = rbind(N[[i]], m2[[i]])
    }
  }
  
  if (type=="exp"){
    for (i in seq_along(m)){
      cis = exp(confint(models[[i]], alpha=1-level))
      m2[[i]] =
        data.frame(
          "Est"   = round(exp(m[[i]][,"Est"]),3), 
          "Lower" = round(cis[,1],2),
          "Upper" = round(cis[,2],2),
          "names" = rownames(m[[i]]))
      N[[i]] = data.frame("Est"=obs[[i]], "Lower"=NA, "Upper"=NA, "names" = "N")
      m2[[i]] = rbind(N[[i]], m2[[i]])
    }
  }
  
  ## Formatting of Table ##
  options(warn=-1)
  # Reduce repeats the function provided over all the elements of m2
  merged = Reduce(function(...) merge(..., by="names", all=TRUE), m2)
  names(merged) = c("Variables", rep(model.names, each=3))
  merged[,c(seq(3, length.out=length(model.names), by=3))] <- sapply(merged[,c(seq(3, length.out=length(model.names), by=3))], as.character)
  merged[is.na(merged)] <- ""
  Note = paste("Sig Levels: *** < 0.001, ** < 0.01, * < 0.05")
  return(list("Table"=merged,"Note"=Note))
}

