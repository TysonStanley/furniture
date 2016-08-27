
table1 = function(data, vars, splitby=NULL, splitby_labels = NULL, test=FALSE, test.type="default", piping = FALSE,
                  rounding=3, var.names=NULL, format.output="full", NAkeep = FALSE, m_label = "Missing"){
  
  # == # Checks and Data # == #
  
  if (NAkeep)
    NAkeep = "always"
  else
    NAkeep = "no"
  
  data = as.data.frame(data)
  d = as.data.frame(data[, vars])
  
  ### Naming of variables
  if (!is.null(var.names)){
    stopifnot(length(var.names)==length(names(d)))
    names(d) = var.names
  } else {
    names(d) = vars
  }
  
  ### Splitby Variable
  if (is.null(splitby)){
    data$splitby = 1
  } else {
    data$splitby = data[, splitby]
  }
  d$split = droplevels(as.factor(data$splitby))
  if (test & length(levels(data$splitby))>1)
    test = TRUE
  else
    test = FALSE
  
  if (!is.null(splitby_labels))
    levels(d$split) = splitby_labels
  
  N = t(tapply(d[,1], d$split, length))
  
  # == # Summarizing Data # == # 
  
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
    
    
    # == # Optional Odds Ratio Table # == #
    
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
  
  if (length(levels(d$split)) == 1){
    names(final)[2] = "Mean/Count (SD/%)"
  }
  
  final_l = list(final)
  class(final_l) = c("table1", "list")
  
  if (piping){
    print(final_l)
    invisible(data)
  } else {
    if (format.output == "stars"){
      cat("Note: p<.05 = *, p<.01 = **, p<.001 ***")
      return(final_l)
    }
    else
      return(final_l)
  }
}

print.table1 <- function(x, ...){
  print(x[[1]], ..., row.names = FALSE)
}

