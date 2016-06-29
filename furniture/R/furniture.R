
table1 = function(data, vars, splitby=NULL, splitby_labels = NULL,
                  test=FALSE, test.type="default", rounding=3, 
                  var.names=NULL, format.output="full", NAkeep = FALSE,
                  m_label = "Missing")
  
  {
  
  if (NAkeep)
    NAkeep = "always"
  else
    NAkeep = "no"
  
  # === # No Split # === #
  
  if (is.null(splitby)){
    d = as.data.frame(data[, vars])
    
    # Naming of variables
    if (!is.null(var.names)){
      stopifnot(length(var.names)==(length(names(d))))
      names(d) = var.names
    } else {
      names(d) = names(data[, vars]) 
    }
    
    # == # Summarizing the Data # == # 
    tab = tab2 = tests = nams = list()
    for (i in 1:dim(d)[2]){
      nams[[i]] = names(d)[i]
      # If Factor
      if (is.factor(d[,i]) | is.character(d[,i])){
        d[,i] = as.factor(d[,i])
        tab[[i]] = table(d[,i], useNA=NAkeep)
        tab2[[i]] = round(table(d[,i], useNA=NAkeep)/sum(table(d[,i], useNA=NAkeep)), rounding)
      } else if (is.numeric(d[,i]) | is.integer(d[,i])){
        tab[[i]] = round(mean(d[,i], na.rm=TRUE), rounding)
        tab2[[i]] = round(sd(d[,i], na.rm=TRUE), rounding)
      } else {
        paste("Variables need to be either factor or numeric.")
      }
    }
    # == # Formatting Table # == # 
    tabZ = data.frame(matrix(nrow=length(names(table(d[,1], useNA=NAkeep))), ncol=2))
    for (j in 1:length(tab)){
      if (is.factor(d[,j])){
        tabX = data.frame(names(table(d[,j], useNA=NAkeep)))
      } else if (is.numeric(d[,j])){
        tabX = data.frame(paste(" "))
      }
      
      if (is.factor(d[,j])){
        tabX = data.frame(tabX, 
                          paste0(round(tab[[j]],2), " (", round(tab2[[j]]*100,1), "%)"))
      } else if (is.numeric(d[,j]) | is.integer(d[,j])){
        tabX = data.frame(tabX, 
                          paste0(round(tab[[j]][[1]],2), " (", round(tab2[[j]][[1]],2), ")"))
      }
      n3 = data.frame(names(d)[j], matrix(" ", ncol=1, nrow=1))
      names(tabZ) = names(tabX) = names(n3) = c(" ", "Means (SD)")
      tabW = rbind(n3, tabX)
      tabZ = rbind(tabZ, tabW)
    }
    N = data.frame("Observations", dim(d)[1])
    names(N) = c(" ", "Means (SD)")
    rem = ifelse(is.na(tabZ[,2]), FALSE, TRUE)
    final = tabZ[rem,]
    final$` ` = as.character(final$` `)
    final$` `[is.na(final$` `)] = m_label
    final = rbind(N, final)
    
    return(list("Table1"=final))
    
    
    
    # === # Stratify by Split # === #
    
  } else if (!is.null(splitby)){
    d = as.data.frame(data[, vars])
    
    # Naming of variables
    if (!is.null(var.names)){
      stopifnot(length(var.names)==length(names(d)))
      names(d) = var.names
    } else {
      names(d) = names(data[, vars]) 
    }
    
    N = t(tapply(d[,1], data[, splitby], length))

    d$split = as.factor(data[, splitby])
    if (!is.null(splitby_labels))
      levels(d$split) = splitby_labels
    
    # == # Splitting the data # == # 
    tab = tab2 = tests = tests2 = nams = list()
    for (i in 1:(dim(d)[2]-1)){
      nams[[i]] = names(d)[i]
      # If Factor
      if (is.factor(d[,i])){
        tab[[i]] = tapply(d[,i], d$split, table, useNA=NAkeep)
        tab2[[i]] = tapply(d[,i], d$split, function(x) round(table(x, useNA=NAkeep)/sum(table(x, useNA=NAkeep)), rounding))
        if (test)
          tests[[i]] = chisq.test(d$split, d[,i])
        if (test & test.type=="or")
          tests2[[i]] = glm(d$split ~ d[, i], family=binomial(link="logit"))
      # If Numeric
      } else if (is.numeric(d[,i]) | is.integer(d[,i])){
        tab[[i]] = round(tapply(d[,i], d$split, mean, na.rm=TRUE), rounding)
        tab2[[i]] = round(tapply(d[,i], d$split, sd, na.rm=TRUE), rounding)
        if (length(levels(d$split))>2 & test){
          lt = car::leveneTest(y=d[,i], group=d$split)$`Pr(>F)`[1]
          if (lt<0.05){
            # Performs an approximate method of Welch (1951)
            tests[[i]] = oneway.test(d[,i] ~ d$split, var.equal=FALSE)
          } else {
            # Performs a simple one-way ANOVA
            tests[[i]] = oneway.test(d[,i] ~ d$split, var.equal=TRUE)
          }
        } else if (test){
          tests[[i]] = t.test(d[,i] ~ d$split)        
        } 
        
        if (test & test.type=="or"){
          tests2[[i]] = glm(d$split ~ d[, i], family=binomial(link="logit"))
        }
      } else {
        paste("Variables need to be either factor or numeric.")
      }
    }
  
  
    # == # Formatting Table # == # 
    if (test){
      if (test.type=="or"){
        OR = data.frame(matrix(nrow=length(levels(d[,i]))+1, ncol=4))
        names(OR) = c(" ", "OR", "Lower", "Upper")
      }

      if (format.output=="full")
        tabZ = data.frame(matrix(nrow=length(levels(d[,i])), ncol=length(levels(d$split))+3))
      else if (format.output=="pvalue" | format.output=="stars")
        tabZ = data.frame(matrix(nrow=length(levels(d[,i])), ncol=length(levels(d$split))+2))
    } else {
      tabZ = data.frame(matrix(nrow=length(levels(d[,i])), ncol=length(levels(d$split))+1))
    }

    for (j in 1:length(tab)){
      if (is.factor(d[,j])){
        tabX = data.frame(names(table(d[,j], useNA=NAkeep)))
      } else if (is.numeric(d[,j])){
        tabX = data.frame(paste(" "))
      }
      for (i in 1:length(levels(d$split))){
        if (is.factor(d[,j])){
          tabX = data.frame(tabX, 
                            paste0(round(tab[[j]][[i]],2), " (", round(tab2[[j]][[i]]*100,1), "%)"))
        } else if (is.numeric(d[,j])){
          tabX = data.frame(tabX, 
                            paste0(round(tab[[j]][[i]],2), " (", round(tab2[[j]][[i]],2), ")"))
        }
      }
      
      
      # == # Odds Ratio Table # == #
      if (test & test.type == "or" & NAkeep == "no"){
        cis = exp(confint(tests2[[j]]))
        or  = exp(tests2[[j]]$coef)
        if (is.numeric(d[,j])){
          n4  = data.frame("", 
                           round(or[-1],2),
                           round(cis[-1,1],2),
                           round(cis[-1,2],2))
        } else if (is.factor(d[,j])){
          n4  = data.frame("", 
                           c(1, round(or[-1],2)),
                           c(1, round(cis[-1,1],2)),
                           c(1, round(cis[-1,2],2)))
        }
        tabQ = data.frame("", "", "", "")
        tabQ[] = sapply(tabQ, as.character)
        names(n4) = names(tabQ) = c(" ", "OR", "Lower", "Upper")
        n5 = rbind(tabQ, n4)
        OR = rbind(OR, n5)
        rem2 = ifelse(is.na(OR[,1]), FALSE, TRUE)
        OR = OR[rem2,]
      } else {
        OR = NULL
      }
      
      ## If test == TRUE, tests of comparisons by split ##
      
      if (test & format.output=="full"){
        if (is.factor(d[,j])){
          n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                          paste("Chi Square:", round(tests[[j]]$statistic,2)), 
                          paste(round(tests[[j]]$p.value,3)))
        } else if (is.numeric(d[,j])){
          if (length(levels(d$split))>2){
            n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                            paste("F-Value:", round(tests[[j]]$statistic[[1]],2)), 
                            paste(round(tests[[j]]$p.value[1],3)))
          } else {
            n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                            paste("T-Test:", round(tests[[j]]$statistic[[1]],2)), 
                            paste(round(tests[[j]]$p.value,3)))
          }
        }
        tabX = data.frame(tabX, "", "")
        names(tabZ) = names(tabX) = names(n3) = c(" ", levels(d$split), "Test", "P-Value")
        tabW = rbind(n3, tabX)
        tabZ = rbind(tabZ, tabW)
        
      } else if (test & format.output=="pvalue"){
        if (is.factor(d[,j])){
          n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1),
                          paste(round(tests[[j]]$p.value,3)))
        } else if (is.numeric(d[,j])){
          if (length(levels(d$split))>2){
            n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                            paste(round(tests[[j]]$p.value[1],3)))
          } else {
            n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                            paste(round(tests[[j]]$p.value,3)))
          }
        }
        tabX = data.frame(tabX, "")
        names(tabZ) = names(tabX) = names(n3) = c(" ", levels(d$split), "P-Value")
        tabW = rbind(n3, tabX)
        tabZ = rbind(tabZ, tabW)
        
      } else if (test & format.output=="stars"){
          n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1),
                          paste( ifelse(tests[[j]]$p.value < 0.001, "***", 
                                 ifelse(tests[[j]]$p.value < 0.01,  "**", 
                                 ifelse(tests[[j]]$p.value < 0.05,  "*", "")))))
        tabX = data.frame(tabX, "")
        names(tabZ) = names(tabX) = names(n3) = c(" ", levels(d$split), "")
        tabW = rbind(n3, tabX)
        tabZ = rbind(tabZ, tabW)
        
      } else {
        n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1))
        names(tabZ) = names(tabX) = names(n3) = c(" ", levels(d$split))
        tabW = rbind(n3, tabX)
        tabZ = rbind(tabZ, tabW)
      }
    }
    
    # == # Observations # == #
    if (format.output=="full" & test){
      N = data.frame("Observations", N, "", "")
      names(N) = c(" ", levels(d$split), "Test", "P-Value")
    } else if ((format.output=="pvalues" | format.output=="stars") & test){
      N = data.frame("Observations", N, "") 
      if (format.output=="pvalues")
        names(N) = c(" ", levels(d$split), "P-Value")
      else 
        names(N) = c(" ", levels(d$split), "")
    } else {
      N = data.frame("Observations", N)
      names(N) = c(" ", levels(d$split))
    }
    
    tabZ = rbind(N, tabZ)
    rem  = ifelse(is.na(tabZ[,2]), FALSE, TRUE)
    final = tabZ[rem,]
    if (!is.null(OR)){
      OR = rbind(tabQ, OR)
      final = cbind(final, OR)
      names(final)[4] = " "
    }
    final$` ` = as.character(final$` `)
    final$` `[is.na(final$` `)] = m_label
    

    # === # FINAL OUTPUT # === #
    if (format.output == "stars"){
      noted = paste("Note: p<.05 = *, p<.01 = **, p<.001 ***")
      return(list("Table1"=final, "Note"=noted))
    }
    else
      return(list("Table1"=final))
  }
}

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
