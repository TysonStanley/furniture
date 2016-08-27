
tableX = function(..., model.names, type="default", level=.95){

  ## Error Management ##
  if (type!="default" & type!="exp") 
    stop(cat(paste("The", type, "method is not supported. Yet. Let me know you want it @ t.barrett@aggiemail.usu.edu.",
                   "\n","Types Supported: default (reports estimates and standard errors) 
                   and exp (reports exponentiated estimates and confidence intervals.")))
  for (i in c(...)){
    if (class(i) == "lm" | class(i) == "glm")
      stop(cat(paste("The models must be 'lm' or 'glm' objects. If there is another modeling type 
                   let me know you want it @ t.barrett@aggiemail.usu.edu.")))
  }
  if (is.null(model.names)) 
    stop(cat(paste("Need model.names argument. Can be any string (no illegal characters).")))
  
  ## Initial Model Summaries ##
  models = list(...)
  m = m2 = rows = pval = se = obs = N = list()
  for (i in 1:length(models)){
    summed      = summary(models[[i]])$coefficients
    m[[i]]      = data.frame("Est"      = summed[,1])
    se[[i]]     = data.frame("SE"       = summed[,2])
    pval[[i]]   = data.frame("PValue"   = summed[,4]) 
    obs[[i]]    = data.frame("Est" = sum(!is.na(models[[i]]$fitted.values)))
  }
  
  ## Specific Types
  if (type=="default"){
    for (i in seq_along(m)){
      m2[[i]] =
        data.frame(
          "Est"   = round(m[[i]], 3), 
          "SE"    = round(se[[i]], 2),
          "Sig"   = paste(ifelse(pval[[i]] < .001, "***",
                          ifelse(pval[[i]] < .01, "**",
                          ifelse(pval[[i]] < .05, "*", "")))),
          "names" = rownames(m[[i]]))
      N[[i]] = data.frame("Est"=round(obs[[i]]), "SE"=NA, "Sig"=NA, "names" = "N")
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
  if (type == "default"){
    for (j in 1:length(m2)){
      names(m2[[j]]) = c(model.names[[j]], paste0("SE", j), paste0("Sig", j), "names")
    }
  } else {
    for (j in 1:length(m2)){
      names(m2[[j]]) = c(model.names[[j]], paste0("Lower", j), paste0("Upper", j), "names")
    }
  }
  merged = Reduce(function(...) merge(..., by="names", all=TRUE), m2)
  names(merged)[1] = c("Variables")
  merged[,2:dim(merged)[2]] = sapply(merged[,2:dim(merged)[2]], as.character)
  merged = furniture::washer(merged, is.na, value="")
  Note = paste("Sig Levels: *** < 0.001, ** < 0.01, * < 0.05")
  
  if (type == "default")
    return(list("Table"=merged,"Note"=Note))
  else
    return(list("Table1"=merged))
}

