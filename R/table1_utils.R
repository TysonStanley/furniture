# Selecting Function
selecting <- function(d_, ...) {
  listed <- eval(substitute(alist(...)))
  
  ## Return all variables
  if (length(listed) == 0)
    return(d_)
  ## If input are indices
  if (length(listed) == 1 && any(grepl("^c\\(.*\\)$", listed) & length(listed[[1]]) != length(d_[[1]]))){
    return(d_[, eval(listed[[1]]), drop = FALSE])
  } else if (length(listed) >= 1){
    ## Data Frame
    df <- lapply(seq_along(listed), 
                 function(i) eval(listed[[i]], d_))
    ## data frame with original row names
    df <- data.frame(df, stringsAsFactors = TRUE)
  } else {
    stop("Something went wrong with the variables listed", call. = FALSE)
  }
  
  ## Variable Names
  names1 <- names(listed)
  names(df) <- sapply(seq_along(listed), function(x) to_name(listed, names1, x))
  
  ## Remove any empty rows and Add attribute for splitby to work
  empty_rows <- which(apply(df, 1, function(x) all(is.na(x))))
  if (length(empty_rows) == 0){
    attr(df, "empty_rows") <- NULL
  } else {
    attr(df, "empty_rows") <- empty_rows
  }
  
  ## Returned data.frame
  df
}



## Used in the selecting function
to_name <- function(listed, names1, i) {
  if (is.null(names1)) {
    deparse(listed[[i]])
  } else {
    if (names1[[i]] == "") {
      deparse(listed[[i]])
    } else {
      names1[[i]]
    }
  }
}





## Does the summary of table1
table1_summarizing <- function(d, num_fun, num_fun2, second, row_wise, test, param, NAkeep, total){
  ## Summarizing The Data
  d <- data.frame(d, stringsAsFactors = TRUE)
  tab <- tab2 <- tests <- nams <- list()
  nam <- names(d)

  ## Track heteroskedasticity warnings
  hetero_vars <- character(0)
  
  for (i in 1:(dim(d)[2]-1)){
    nams[[i]] <- names(d)[i]
    ## If character
    if (is.character(d[[i]])){
      d[[i]] <- factor(d[[i]])
    }
    
    ## Factor ##
    if (is.factor(d[,i])){
      ## if total is TRUE, add it to the split statistics
      if(isTRUE(total)) { 
        tab[[i]] <- c(list("Total" = table(d[[i]], useNA=NAkeep)), tapply(d[[i]], d$split, table, useNA=NAkeep))
      } else { 
        tab[[i]] <- tapply(d[[i]], d$split, table, useNA=NAkeep)
      }
      
      if (!row_wise){
        if(isTRUE(total)) { 
          tab2[[i]] <- c(list("Total" = table(d[[i]], useNA=NAkeep)/sum(table(d[[i]], useNA=NAkeep))), 
                         tapply(d[[i]], d$split, function(x) table(x, useNA=NAkeep)/sum(table(x, useNA=NAkeep))))
        } else { 
          tab2[[i]] <- tapply(d[[i]], d$split, function(x) table(x, useNA=NAkeep)/sum(table(x, useNA=NAkeep)))
        }
        
      } else if (row_wise){
        if(isTRUE(total)) { 
          tab2[[i]] <- c(list("Total" = table(d[[i]], useNA=NAkeep)/table(d[,i], useNA=NAkeep)), 
                         tapply(d[[i]], d$split, function(x) table(x, useNA=NAkeep)/table(d[,i], useNA=NAkeep)))
        } else { 
          tab2[[i]] <- tapply(d[[i]], d$split, function(x) table(x, useNA=NAkeep)/table(d[,i], useNA=NAkeep))
        }
        
      } else {
        stop("'rowwise' argument must be TRUE or FALSE", call. = FALSE)
      }
      if (test)
        tests[[i]] <- chisq.test(d$split, d[[i]])
      
      
      ## Numeric ##
    } else if (is.numeric(d[[i]])){
      ## Function 1
      if (!nams[[i]] %in% second){
        if(isTRUE(total)) { 
          tab[[i]] <- c(list("Total" = num_fun(d[[i]])), tapply(d[[i]], d$split, num_fun))
        } else { 
          tab[[i]] <- tapply(d[[i]], d$split, num_fun)
        }
        
        ## Function 2
      } else if (nams[[i]] %in% second){
        if(isTRUE(total)) { 
          tab[[i]] <- c(list("Total" = num_fun2(d[[i]])), tapply(d[[i]], d$split, num_fun2))
        } else { 
          tab[[i]] <- tapply(d[[i]], d$split, num_fun2)
        }
        
      } else {
        stop("variable(s) in 'second' not found", call. = FALSE)
      }
      
      if (test){
        ## Breusch-Pagan Test of Heteroskedasticity (equality of variances)
        comp <- complete.cases(d[[i]], d$split)
        resids <- resid(lm(d[comp, i] ~ d$split[comp]))^2
        r2 <- summary(lm(resids ~ d$split[comp]))$r.squared
        lt <- dchisq(length(resids)*r2, df = length(levels(d$split)))
      }
      
      ## Parametric or Non-parametric
      if (param){
        param_result <- parametric(d[[i]] ~ d$split, d$split, lt, test, nam, i)
        tests[[i]] <- param_result$test
        if (param_result$hetero) {
          hetero_vars <- c(hetero_vars, nam[i])
        }
      } else {
        tests[[i]] <- nonparametric(d[[i]] ~ d$split)
      }
    }
  }

  ## Print consolidated heteroskedasticity message
  if (length(hetero_vars) > 0) {
    message("Breusch-Pagan Test of Heteroskedasticity suggests `var.equal = FALSE` for: ",
            paste(hetero_vars, collapse = ", "))
  }

  invisible(list(tab, tab2, tests, nams))
}


parametric <- function(formula, split, lt, test, nam, i){

  hetero_detected <- FALSE
  test_result <- NULL

  ## For splitby vars with more than 2 levels
  if (length(levels(split))>2 && test){
    if (lt<0.05){
      hetero_detected <- TRUE
      ## Performs an approximate method of Welch (1951)
      test_result <- tryCatch(
        oneway.test(formula, var.equal=FALSE),
        error = function(cond){
          message(cond, "\n")
          return(NA)
        }
      )
    } else {
      ## Performs a simple one-way ANOVA
      test_result <- tryCatch(
        oneway.test(formula, var.equal=TRUE),
        error = function(cond){
          message(cond, "\n")
          return(list(statistic = NA,
                      p.value = NA))
        }
      )
    }

  } else if (test){
    if (lt<0.05){
      hetero_detected <- TRUE
      ## Performs an approximate method of Welch (1951)
      test_result <- tryCatch(
        t.test(formula, var.equal=FALSE),
        error = function(cond){
          message(cond, "\n")
          return(list(statistic = NA,
                      p.value = NA))
        }
      )
    } else {
      ## Performs a simple t-test
      test_result <- tryCatch(
        t.test(formula, var.equal=TRUE),
        error = function(cond){
          message(cond, "\n")
          return(list(statistic = NA,
                      p.value = NA))
        }
      )
    }
  }

  return(list(test = test_result, hetero = hetero_detected))

}

nonparametric <- function(formula){
  tryCatch(
    kruskal.test(formula),
    error = function(cond){
      message(cond)
      return(list(statistic = NA,
                  p.value = NA))
    }
  )
}


## Formatting of table1 with no condense
table1_format_nocondense = function(d, tab, tab2, tests, test, NAkeep, rounding_perc,
                                    format_output, second, nams, simple, output, big_mark, total, param){
  d <- as.data.frame(d, stringsAsFactors = TRUE)
  
  if (isTRUE(total)){
    tot <- 1
    nams <- c(" ", "Total", levels(d$split))
  } else {
    tot <- 0
    nams <- c(" ", levels(d$split))
  }
  
  if (test){
    if (grepl("f|F", format_output))
      tabZ <- data.frame(matrix(nrow=0, ncol=length(levels(d$split))+3+tot), stringsAsFactors = TRUE)
    else if (grepl("p|P", format_output) || grepl("s|S", format_output))
      tabZ <- data.frame(matrix(nrow=0, ncol=length(levels(d$split))+2+tot), stringsAsFactors = TRUE)
  } else {
    tabZ <- data.frame(matrix(nrow=0, ncol=length(levels(d$split))+1+tot), stringsAsFactors = TRUE)
  }
  
  for (j in seq_along(tab)){
    if (is.factor(d[,j])){
      if (!grepl("^t", output)){
        tabX <- data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))), stringsAsFactors = TRUE)
      } else {
        tabX <- data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))), stringsAsFactors = TRUE)
      }
    } else if (is.numeric(d[,j])){
      tabX <- data.frame(paste(" "), stringsAsFactors = TRUE)
    } else {
      stop(paste("Problem with variable", names(d)[j]))
    }
    
    ## Factor
    len <- length(levels(d$split)) + tot
    for (i in 1:len){
      if (is.factor(d[,j])){
        if (!simple){
          tabX <- data.frame(tabX, 
                             paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = big_mark)), " (", 
                                    round(tab2[[j]][[i]]*100, rounding_perc), "%)"), 
                             stringsAsFactors = TRUE)
        } else if (simple){
          tabX <- data.frame(tabX, 
                             paste0(round(tab2[[j]][[i]]*100, rounding_perc), "%"), 
                             stringsAsFactors = TRUE)
        }
        
        ## Numeric
      } else if (is.numeric(d[,j])){
        tabX <- data.frame(tabX, tab[[j]][[i]], stringsAsFactors = TRUE)
      } else {
        stop(paste("Problem with variable", names(d)[j]))
      }
    }
    
    the_test_label <- ifelse(rep(param, 2), c("F-Value:", "T-Test:"), c("Kruskal-Wallis:", "Kruskal-Wallis:"))
    
    ## If test == TRUE, tests of comparisons by split ##
    if (test && grepl("f|F", format_output)){
      if (is.factor(d[,j])){
        n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split))+tot, nrow=1), 
                         paste("Chi Square:", round(tests[[j]]$statistic,2)), 
                         paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))), stringsAsFactors = TRUE)
      } else if (is.numeric(d[,j])){
        if (length(levels(d$split))>2){
          n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split))+tot, nrow=1), 
                           paste(the_test_label[1], round(tests[[j]]$statistic[[1]],2)), 
                           paste(ifelse(tests[[j]]$p.value[1] < .001, "<.001", round(tests[[j]]$p.value[1],3))), stringsAsFactors = TRUE)
        } else {
          n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split))+tot, nrow=1), 
                           paste(the_test_label[2], round(tests[[j]]$statistic[[1]],2)), 
                           paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))), stringsAsFactors = TRUE)
        }
      }
      tabX <- data.frame(tabX, "", "", stringsAsFactors = TRUE)
      names(tabZ) = names(tabX) = names(n3) = c(nams, "Test", "P-Value")
      tabW <- rbind(n3, tabX)
      tabZ <- rbind(tabZ, tabW)
      
    } else if (test && grepl("p|P", format_output)){
      if (is.factor(d[,j])){
        n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split))+tot, nrow=1),
                         paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))), stringsAsFactors = TRUE)
      } else if (is.numeric(d[,j])){
        if (length(levels(d$split))>2){
          n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split))+tot, nrow=1), 
                           paste(ifelse(tests[[j]]$p.value[1] < .001, "<.001", round(tests[[j]]$p.value[1],3))), stringsAsFactors = TRUE)
        } else {
          n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split))+tot, nrow=1), 
                           paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))), stringsAsFactors = TRUE)
        }
      }
      tabX <- data.frame(tabX, "", stringsAsFactors = TRUE)
      names(tabZ) = names(tabX) = names(n3) = c(nams, "P-Value")
      tabW <- rbind(n3, tabX)
      tabZ <- rbind(tabZ, tabW)
      
    } else if (test && grepl("s|S", format_output)){
      n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split))+tot, nrow=1),
                       paste( ifelse(tests[[j]]$p.value < 0.001, "***", 
                              ifelse(tests[[j]]$p.value < 0.01,  "**", 
                              ifelse(tests[[j]]$p.value < 0.05,  "*", "")))), stringsAsFactors = TRUE)
      tabX <- data.frame(tabX, "", stringsAsFactors = TRUE)
      names(tabZ) = names(tabX) = names(n3) = nams
      tabW <- rbind(n3, tabX)
      tabZ <- rbind(tabZ, tabW)
      
    } else {
      n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split))+tot, nrow=1), stringsAsFactors = TRUE)
      names(tabZ) = names(tabX) = names(n3) = nams
      tabW <- rbind(n3, tabX)
      tabZ <- rbind(tabZ, tabW)
    }
  }
  invisible(tabZ)
}

## Formatting of table1 with condense
table1_format_condense = function(d, tab, tab2, tests, test, NAkeep, rounding_perc,
                                  format_output, second, nams, simple, output, big_mark, total){
  d <- as.data.frame(d, stringsAsFactors = TRUE)
  
  if (isTRUE(total)){
    tot <- 1
    nams <- c(" ", "Total", levels(d$split))
  } else {
    tot <- 0
    nams <- c(" ", levels(d$split))
  }
  
  if (test){
    if (grepl("p|P", format_output) || grepl("s|S", format_output))
      tabZ <- data.frame(matrix(nrow=0, ncol=length(levels(d$split))+2+tot), stringsAsFactors = TRUE)
  } else {
    tabZ <- data.frame(matrix(nrow=0, ncol=length(levels(d$split))+1+tot), stringsAsFactors = TRUE)
  }
  
  for (j in seq_along(tab)){
    if (is.factor(d[,j])){
      if (length(levels(d[,j])) == 2){
        if (!grepl("text", output)){
          tabX <- data.frame(paste0(names(d)[j], ": ", names(table(d[,j], useNA=NAkeep)[2])), stringsAsFactors = TRUE)
        } else {
          tabX <- data.frame(paste0(names(d)[j], ": ", names(table(d[,j], useNA=NAkeep)[2])), stringsAsFactors = TRUE)
        }
      } else if (length(levels(d[,j])) > 2){
        if (!grepl("text", output)){
          tabX <- data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))), stringsAsFactors = TRUE)
        } else {
          tabX <- data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))), stringsAsFactors = TRUE)
        }
      }
    } else if (is.numeric(d[,j])){
      tabX <- data.frame(paste(names(d)[j]), stringsAsFactors = TRUE)
    } else {
      stop(paste("Problem with variable", names(d)[j]))
    }
    
    ## Counts and Percentages or Just Percentages
    len <- length(levels(d$split)) + tot
    for (i in 1:len){
      if (is.factor(d[,j])){
        ## Just percentages
        if (simple){
          if (length(levels(d[,j])) == 2){
            tabX <- data.frame(tabX, 
                               paste0(round(tab2[[j]][[i]][2]*100, 1), "%"), stringsAsFactors = TRUE)
          } else {
            tabX <- data.frame(tabX, 
                               paste0(round(tab2[[j]][[i]]*100, 1), "%"), stringsAsFactors = TRUE)
          }
          ## Counts and Percentages
        } else {
          if (length(levels(d[,j])) == 2){
            tabX <- data.frame(tabX, 
                               paste0(suppressWarnings(formatC(tab[[j]][[i]][2], big.mark = big_mark)), " (", 
                                      round(tab2[[j]][[i]][2]*100, 1), "%)"), stringsAsFactors = TRUE)
          } else {
            tabX <- data.frame(tabX, 
                               paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = big_mark)), " (", 
                                      round(tab2[[j]][[i]]*100, 1), "%)"), stringsAsFactors = TRUE)
          }
        }
        
        
      } else if (is.numeric(d[,j])){
        tabX <- data.frame(tabX, tab[[j]][[i]], stringsAsFactors = TRUE)
      }
    }
    
    ## If test == TRUE, tests of comparisons by split ##
    if (test){
      if (is.factor(d[,j])){
        if (length(levels(d[,j])) == 2){
          n3 <- data.frame(tabX, 
                           paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))), stringsAsFactors = TRUE)
          tabX <- n3
        } else {
          blankX <- data.frame(names(d)[j], 
                               matrix(" ", ncol=(length(levels(d$split))+tot), nrow = 1),
                               paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))), stringsAsFactors = TRUE)
          n3 <- data.frame(tabX, " ", stringsAsFactors = TRUE)
          names(blankX) <- names(n3)
          tabX <- rbind(blankX, n3)
        }
      } else if (is.numeric(d[,j])){
        if (length(levels(d$split))>2){
          n3 <- data.frame(tabX, 
                           paste(ifelse(tests[[j]]$p.value[1] < .001, "<.001", round(tests[[j]]$p.value[1],3))), stringsAsFactors = TRUE)
          tabX <- n3
        } else {
          n3 <- data.frame(tabX, 
                           paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))), stringsAsFactors = TRUE)
          tabX <- n3
        }
      }
      names(tabZ) = names(tabX) = c(nams, "P-Value")
      tabZ <- rbind(tabZ, tabX)
      
    } else if (!test){
      if (is.factor(d[,j])){
        if (length(levels(d[,j])) == 2){
          ## Nothing
        } else {
          blankX <- data.frame(names(d)[j], matrix(" ", ncol=(length(levels(d$split))+tot), nrow = 1), stringsAsFactors = TRUE)
          n3 <- data.frame(tabX, stringsAsFactors = TRUE)
          names(blankX) <- names(n3)
          tabX <- rbind(blankX, n3)
        }
      } else if (is.numeric(d[,j])){
        if (length(levels(d$split))>2){
          ## Nothing
        } else {
          ## Nothing
        }
      }
      
      names(tabZ) = names(tabX) = nams
      tabZ <- rbind(tabZ, tabX)
    }
  }
  invisible(tabZ)
}
