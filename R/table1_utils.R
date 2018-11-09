#' Selecting Function
#' 
#' For internal use in \code{table1()} and \code{tableC()} to extract the right data. 
#' Can also be used much like \code{dplyr::select()}, although I'd recommend
#' one to use \code{dplyr::select()} in general.
#' 
#' @param d_ the data.frame
#' @param ... the variables
#' 
#' @return The data.frame with the selected variables
#'
#' @export
selecting <- function(d_, ...) {
  listed <- eval(substitute(alist(...)))
  
  ## Return all variables
  if (length(listed) == 0)
    return(d_)
  ## If input are indices
  if (length(listed) == 1 & any(grepl("^c\\(.*\\)$", listed) & length(listed[[1]]) != length(d_[[1]]))){
    return(d_[, eval(listed[[1]])])
  } else if (length(listed) >= 1){
    ## Data Frame
    df <- lapply(seq_along(listed), 
                 function(i) eval(listed[[i]], d_))
    ## data frame with original row names
    df <- data.frame(df)
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
table1_summarizing = function(d, num_fun, num_fun2, second, row_wise, test, NAkeep){
  ## Summarizing The Data
  d <- data.frame(d)
  tab = tab2 = tests = tests2 = nams = list()
  nam <- names(d)
  
  for (i in 1:(dim(d)[2]-1)){
    nams[[i]] <- names(d)[i]
    ## If character
    if (is.character(d[[i]])){
      d[[i]] <- factor(d[[i]])
    }
    
    ## Factor ##
    if (is.factor(d[,i])){
      tab[[i]] <- tapply(d[[i]], d$split, table, useNA=NAkeep)
      if (!row_wise){
        tab2[[i]] <- tapply(d[[i]], d$split, function(x) table(x, useNA=NAkeep)/sum(table(x, useNA=NAkeep)))
      } else if (row_wise){
        tab2[[i]] <- tapply(d[[i]], d$split, function(x) table(x, useNA=NAkeep)/table(d[,i], useNA=NAkeep))
      } else {
        stop("'rowwise' argument must be TRUE or FALSE", call. = FALSE)
      }
      if (test)
        tests[[i]] <- chisq.test(d$split, d[[i]])
    
    ## Numeric ##
    } else if (is.numeric(d[[i]])){
      ## Function 1
      if (!nams[[i]] %in% second){
        tab[[i]] <- tapply(d[[i]], d$split, num_fun)
        ## Function 2
      } else if (nams[[i]] %in% second){
        tab[[i]] <- tapply(d[[i]], d$split, num_fun2)
      } else {
        stop("variable(s) in 'second' not found", call. = FALSE)
      }
      
      ## For splitby vars with more than 2 levels
      if (length(levels(d$split))>2 & test){
        ## Breusch-Pagan Test of Heteroskedasticity (equality of variances)
        comp   <- complete.cases(d[[i]], d$split)
        resids <- resid(lm(d[comp,i] ~ d$split[comp]))^2
        r2     <- summary(lm(resids ~ d$split[comp]))$r.squared
        lt     <- dchisq(length(resids)*r2, df = length(levels(d$split)))
        if (lt<0.05){
          ## Performs an approximate method of Welch (1951)
          tests[[i]] <- oneway.test(d[[i]] ~ d$split, var.equal=FALSE)
          message(paste0("Breusch-Pagan Test of Heteroskedasticity suggests `var.equal = FALSE` in oneway.test() for: ", nam[i]))
        } else {
          ## Performs a simple one-way ANOVA
          tests[[i]] <- oneway.test(d[[i]] ~ d$split, var.equal=TRUE)
        }
      } else if (test){
        ## Breusch-Pagan Test of Heteroskedasticity (equality of variances)
        comp   <- complete.cases(d[[i]], d$split)
        resids <- resid(lm(d[comp,i] ~ d$split[comp]))^2
        r2     <- summary(lm(resids ~ d$split[comp]))$r.squared
        lt     <- dchisq(length(resids)*r2, df = length(levels(d$split)))
        if (lt<0.05){
          ## Performs an approximate method of Welch (1951)
          tests[[i]] <- t.test(d[[i]] ~ d$split, var.equal=FALSE)
          message(paste0("Breusch-Pagan Test of Heteroskedasticity suggests `var.equal = FALSE` in t.test() for: ", nam[i]))
        } else {
          ## Performs a simple one-way ANOVA
          tests[[i]] <- t.test(d[[i]] ~ d$split, var.equal=TRUE)
        }    
      }
      
    } else {
      stop("Variables need to be either factor, character or numeric.", call. = FALSE)
    }
  }
  
  invisible(list(tab, tab2, tests, nams))
}

## Formatting of table1 with no condense
table1_format_nocondense = function(d, tab, tab2, tests, test, NAkeep, rounding_perc, format_output, second, nams, simple, output, f1){
  d <- as.data.frame(d)
  if (test){
    if (grepl("f|F", format_output))
      tabZ <- data.frame(matrix(nrow=0, ncol=length(levels(d$split))+3))
    else if (grepl("p|P", format_output) | grepl("s|S", format_output))
      tabZ <- data.frame(matrix(nrow=0, ncol=length(levels(d$split))+2))
  } else {
    tabZ <- data.frame(matrix(nrow=0, ncol=length(levels(d$split))+1))
  }
  
  for (j in 1:length(tab)){
    if (is.factor(d[,j])){
      if (!grepl("^t", output)){
        tabX <- data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))))
      } else {
        tabX <- data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))))
      }
    } else if (is.numeric(d[,j])){
      tabX <- data.frame(paste(" "))
    } else {
      stop(paste("Problem with variable", names(d)[j]))
    }
    
    ## Factor
    for (i in 1:length(levels(d$split))){
      if (is.factor(d[,j])){
        if (!simple){
          tabX <- data.frame(tabX, 
                             paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1)), " (", 
                                    round(tab2[[j]][[i]]*100, rounding_perc), "%)"))
        } else if (simple){
          tabX <- data.frame(tabX, 
                             paste0(round(tab2[[j]][[i]]*100, rounding_perc), "%"))
        }
        
        ## Numeric
      } else if (is.numeric(d[,j])){
        tabX <- data.frame(tabX, tab[[j]][[i]])
      } else {
        stop(paste("Problem with variable", names(d)[j]))
      }
    }
    
    ## If test == TRUE, tests of comparisons by split ##
    if (test & grepl("f|F", format_output)){
      if (is.factor(d[,j])){
        n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                         paste("Chi Square:", round(tests[[j]]$statistic,2)), 
                         paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
      } else if (is.numeric(d[,j])){
        if (length(levels(d$split))>2){
          n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                           paste("F-Value:", round(tests[[j]]$statistic[[1]],2)), 
                           paste(ifelse(tests[[j]]$p.value[1] < .001, "<.001", round(tests[[j]]$p.value[1],3))))
        } else {
          n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                           paste("T-Test:", round(tests[[j]]$statistic[[1]],2)), 
                           paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
        }
      }
      tabX <- data.frame(tabX, "", "")
      names(tabZ) = names(tabX) = names(n3) = c(" ", levels(d$split), "Test", "P-Value")
      tabW <- rbind(n3, tabX)
      tabZ <- rbind(tabZ, tabW)
      
    } else if (test & grepl("p|P", format_output)){
      if (is.factor(d[,j])){
        n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1),
                         paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
      } else if (is.numeric(d[,j])){
        if (length(levels(d$split))>2){
          n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                           paste(ifelse(tests[[j]]$p.value[1] < .001, "<.001", round(tests[[j]]$p.value[1],3))))
        } else {
          n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                           paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
        }
      }
      tabX <- data.frame(tabX, "")
      names(tabZ) = names(tabX) = names(n3) = c(" ", levels(d$split), "P-Value")
      tabW <- rbind(n3, tabX)
      tabZ <- rbind(tabZ, tabW)
      
    } else if (test & grepl("s|S", format_output)){
      n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1),
                       paste( ifelse(tests[[j]]$p.value < 0.001, "***", 
                              ifelse(tests[[j]]$p.value < 0.01,  "**", 
                              ifelse(tests[[j]]$p.value < 0.05,  "*", "")))))
      tabX <- data.frame(tabX, "")
      names(tabZ) = names(tabX) = names(n3) = c(" ", levels(d$split), " ")
      tabW <- rbind(n3, tabX)
      tabZ <- rbind(tabZ, tabW)
      
    } else {
      n3 <- data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1))
      names(tabZ) = names(tabX) = names(n3) = c(" ", levels(d$split))
      tabW <- rbind(n3, tabX)
      tabZ <- rbind(tabZ, tabW)
    }
  }
  invisible(tabZ)
}

## Formatting of table1 with condense
table1_format_condense = function(d, tab, tab2, tests, test, NAkeep, rounding_perc, format_output, second, nams, simple, output, f1){
  d <- as.data.frame(d)
  if (test){
    if (grepl("p|P", format_output) | grepl("s|S", format_output))
      tabZ <- data.frame(matrix(nrow=0, ncol=length(levels(d$split))+2))
  } else {
    tabZ <- data.frame(matrix(nrow=0, ncol=length(levels(d$split))+1))
  }
  
  for (j in 1:length(tab)){
    if (is.factor(d[,j])){
      if (length(levels(d[,j])) == 2){
        if (!grepl("text", output)){
          tabX <- data.frame(paste0(names(d)[j], ": ", names(table(d[,j], useNA=NAkeep)[2])))
        } else {
          tabX <- data.frame(paste0(names(d)[j], ": ", names(table(d[,j], useNA=NAkeep)[2])))
        }
      } else if (length(levels(d[,j])) > 2){
        if (!grepl("text", output)){
          tabX <- data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))))
        } else {
          tabX <- data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))))
        }
      }
    } else if (is.numeric(d[,j])){
      tabX <- data.frame(paste(names(d)[j]))
    } else {
      stop(paste("Problem with variable", names(d)[j]))
    }
    
    ## Counts and Percentages or Just Percentages
    for (i in 1:length(levels(d$split))){
      if (is.factor(d[,j])){
        ## Just percentages
        if (simple){
          if (length(levels(d[,j])) == 2){
            tabX <- data.frame(tabX, 
                               paste0(round(tab2[[j]][[i]][2]*100, 1), "%"))
          } else {
            tabX <- data.frame(tabX, 
                               paste0(round(tab2[[j]][[i]]*100, 1), "%"))
          }
          ## Counts and Percentages
        } else {
          if (length(levels(d[,j])) == 2){
            tabX <- data.frame(tabX, 
                               paste0(suppressWarnings(formatC(tab[[j]][[i]][2], big.mark = f1)), " (", 
                                      round(tab2[[j]][[i]][2]*100, 1), "%)"))
          } else {
            tabX <- data.frame(tabX, 
                               paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1)), " (", 
                                      round(tab2[[j]][[i]]*100, 1), "%)"))
          }
        }
        
        
      } else if (is.numeric(d[,j])){
        tabX <- data.frame(tabX, tab[[j]][[i]])
      }
    }
    
    ## If test == TRUE, tests of comparisons by split ##
    if (test){
      if (is.factor(d[,j])){
        if (length(levels(d[,j])) == 2){
          n3 <- data.frame(tabX, 
                           paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
          tabX <- n3
        } else {
          blankX <- data.frame(names(d)[j], 
                               matrix(" ", ncol=(length(levels(d$split))), nrow = 1),
                               paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
          n3 <- data.frame(tabX, " ")
          names(blankX) <- names(n3)
          tabX <- rbind(blankX, n3)
        }
      } else if (is.numeric(d[,j])){
        if (length(levels(d$split))>2){
          n3 <- data.frame(tabX, 
                           paste(ifelse(tests[[j]]$p.value[1] < .001, "<.001", round(tests[[j]]$p.value[1],3))))
          tabX <- n3
        } else {
          n3 <- data.frame(tabX, 
                           paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
          tabX <- n3
        }
      }
      names(tabZ) = names(tabX) = c(" ", levels(d$split), "P-Value")
      tabZ <- rbind(tabZ, tabX)
      
    } else if (!test){
      if (is.factor(d[,j])){
        if (length(levels(d[,j])) == 2){
          ## Nothing
        } else {
          blankX <- data.frame(names(d)[j], matrix(" ", ncol=(length(levels(d$split))), nrow = 1))
          n3 <- data.frame(tabX)
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
      names(tabZ) = names(tabX) = c(" ", levels(d$split))
      tabZ <- rbind(tabZ, tabX)
    }
  }
  invisible(tabZ)
}
