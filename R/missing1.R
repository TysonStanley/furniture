#' Table 1 for Exploring Missing Data
#' 
#' Produces a descriptive table, just like table1, stratified by whether the observation has missing
#' values on a specified variable. It provides means/frequencies and standard deviations/percentages.
#' Unlike table1, default is test = TRUE. Most other attributes are just like table1.
#' 
#' @param .data the data.frame that is to be analyzed
#' @param ... variables in the data set that are to be compared; unquoted names separated by commas (e.g. age, gender, race) or indices. If indices, it needs to be a single vector (e.g. c(1:5, 8, 9:20) instead of 1:5, 8, 9:20). As it is currently, it CANNOT handle both indices and unquoted names simultaneously.
#' @param miss_var the variable with missing values to be analyzed in formula form (e.g., \code{miss_var = ~gender}) or in quotes (e.g., \code{miss_var = "gender"}); not too surprisingly, it requires that the number missing values > 0.
#' @param row_wise how to calculate percentages for factor variables when \code{miss_var != NULL}: if \code{FALSE} calculates percentages by variable within groups; if \code{TRUE} calculates percentages across groups for one level of the factor variable.
#' @param miss_var_labels allows for custom labels of the miss_var levels; must match the number of levels of the miss_var variable
#' @param test logical; if set to \code{TRUE} then the appropriate bivariate tests of significance are performed
#' @param test_type has two options: "default" performs the default tests of significance only; "or" also give unadjusted odds ratios as well based on logistic regression (only use if splitby has 2 levels)
#' @param piping if \code{TRUE} then the table is printed and the original data is passed on. It is very useful in piping situations where one wants the table but wants it to be part of a larger pipe.
#' @param rounding the number of digits after the decimal for means and SD's; default is 2
#' @param var_names custom variable names to be printed in the table
#' @param format_output has three options: 1) "full" provides the table with the type of test, test statistic, and the p-value for each variable; 2) "pvalues" provides the table with the p-values; and 3) "stars" provides the table with stars indicating significance
#' @param output_type default is "text"; the other options are all format options in the \code{kable()} function in \code{knitr} (e.g., latex, html, markdown, pandoc) as well as "text2" which adds a line below the header in the table.
#' @param format_number default in FALSE; if TRUE, then the numbers are formatted with commas (e.g., 20,000 instead of 20000)
#' @param NAkeep when sset to \code{TRUE} it also shows how many missing values are in the data for each categorical variable being summarized
#' @param m_label when \code{NAkeep = TRUE} this provides a label for the missing values in the table
#' @param booktabs when \code{output_type != "text"}; option is passed to \code{knitr::kable}
#' @param caption when \code{output_type != "text"}; option is passed to \code{knitr::kable}
#' @param align when \code{output_type != "text"}; option is passed to \code{knitr::kable}
#' 
#' @return A table with the number of observations, means/frequencies and standard deviations/percentages is returned. The object is a \code{table1} class object with a print method. Can be printed in \code{LaTex} form.
#'
#' @examples 
#' ## Ficticious Data ##
#' library(furniture)
#' library(tidyverse)
#' 
#' x  <- runif(1000)
#' x  <- sample(c(x, NA), 1000, replace=TRUE)
#' y  <- rnorm(1000)
#' z  <- factor(sample(c(0,1,NA), 1000, replace=TRUE))
#' a  <- factor(sample(c(1,2,NA), 1000, replace=TRUE))
#' df <- data.frame(x, y, z, a)
#' 
#' ## Analyzing Missingness in variable a
#' tableM(df, x, y, z, 
#'          miss_var = ~a)
#' tableM(df, x, y, z,
#'          miss_var = "a")
#' 
#'
#' @export
#' @import stats
#' @importFrom knitr kable
tableM = function(.data, 
                  ..., 
                  miss_var, 
                  row_wise = FALSE, 
                  miss_var_labels = NULL, 
                  test = TRUE, 
                  test_type = "default", 
                  piping = FALSE,
                  rounding = 2, 
                  var_names = NULL, 
                  format_output = "pvalues", 
                  output_type = "text", 
                  format_number = FALSE,
                  NAkeep = FALSE, 
                  m_label = "Missing",
                  booktabs = TRUE, 
                  caption=NULL, 
                  align=NULL){
  
  # == # Checks and Data # == #
  .call = match.call()
  
  if (NAkeep){ 
    NAkeep = "always" 
  } else {
    NAkeep = "no"
  }
  
  if (format_number){
    f1 = ","
  } else {
    f1 = ""
  }
  
  data = table1_(..., d_=.data, .cl=.call)
  d = as.data.frame(data)
  
  ### Naming of variables
  if (!is.null(var_names)){
    stopifnot(length(var_names)==length(names(d)))
    names(d) = var_names
  }
  
  ### miss_var (adds the variable to d as "split")
  if (is.null(miss_var)){
    stop("Need miss_var to be in quoted form or in formula form.")
  } else {
    splitby_ = eval(parse(text = paste(miss_var)[[length(paste(miss_var))]]), .data)
    missby_  = ifelse(is.na(splitby_), 1, 0)
    d$split  = droplevels(factor(missby_, labels = c("Not Missing", "Missing")))
  }
  
  ## Test = TRUE and the splitting variable needs to have more than one level
  if (test & length(levels(d$split))>1){
    test = TRUE
  } else {
    test = FALSE
  }
  
  if (!is.null(miss_var_labels))
    levels(d$split) = miss_var_labels
 
  ## The name of the variable
  missingvariable_ = paste(.call[["miss_var"]])[length(paste(.call[["miss_var"]]))]
  
  ## Number of observations by group
  N = t(tapply(d[,1], d$split, length))
  
  # == # Summarizing Data # == # 
  
  tab = tab2 = tests = tests2 = nams = list()
  for (i in 1:(dim(d)[2]-1)){
    nams[[i]] = names(d)[i]
    ## If character
    if (is.character(d[,i])){
      d[,i] = factor(d[,i])
    }
    
    ## If Factor
    if (is.factor(d[,i])){
      tab[[i]] = tapply(d[,i], d$split, table, useNA=NAkeep)
      if (!row_wise){
        tab2[[i]] = tapply(d[,i], d$split, function(x) table(x, useNA=NAkeep)/sum(table(x, useNA=NAkeep)))
      } else if (row_wise){
        tab2[[i]] = tapply(d[,i], d$split, function(x) table(x, useNA=NAkeep)/table(d[,i], useNA=NAkeep))
      }
      if (test)
        tests[[i]] = chisq.test(d$split, d[,i])
      if (test & test_type=="or")
        tests2[[i]] = glm(d$split ~ d[, i], family=binomial(link="logit"))
    ## If Numeric
    } else if (is.numeric(d[,i]) | is.integer(d[,i])){
      tab[[i]] = round(tapply(d[,i], d$split, mean, na.rm=TRUE), rounding)
      tab2[[i]] = round(tapply(d[,i], d$split, sd, na.rm=TRUE), rounding)
      if (length(levels(d$split))>2 & test){
        ## Breusch-Pagan Test of Heteroskedasticity (equality of variances)
        comp   = complete.cases(d[,i], d$split)
        resids = resid(lm(d[comp,i] ~ d$split[comp]))^2
        r2     = summary(lm(resids ~ d$split[comp]))$r.squared
        lt     = dchisq(length(resids)*r2, df = length(levels(d$split)))
        if (lt<0.05){
          ## Performs an approximate method of Welch (1951)
          tests[[i]] = oneway.test(d[,i] ~ d$split, var.equal=FALSE)
        } else {
          ## Performs a simple one-way ANOVA
          tests[[i]] = oneway.test(d[,i] ~ d$split, var.equal=TRUE)
        }
      } else if (test){
        tests[[i]] = t.test(d[,i] ~ d$split)        
      } 
      
      if (test & test_type=="or"){
        tests2[[i]] = glm(d$split ~ d[, i], family=binomial(link="logit"))
      }
    } else {
      stop("Variables need to be either factor, character or numeric.", .call=FALSE)
    }
  }
  
  
  # == # Formatting Table # == # 
  if (test){
    if (test_type=="or"){
      OR = data.frame(matrix(nrow=length(levels(d[,i]))+1, ncol=4))
      names(OR) = c(" ", "OR", "Lower", "Upper")
    }
    
    if (format_output=="full")
      tabZ = data.frame(matrix(nrow=length(levels(d[,i])), ncol=length(levels(d$split))+3))
    else if (format_output=="pvalues" | format_output=="stars")
      tabZ = data.frame(matrix(nrow=length(levels(d[,i])), ncol=length(levels(d$split))+2))
  } else {
    tabZ = data.frame(matrix(nrow=length(levels(d[,i])), ncol=length(levels(d$split))+1))
  }
  
  for (j in 1:length(tab)){
    if (is.factor(d[,j])){
      if (!grepl("text", output_type)){
        tabX = data.frame(paste("--  ", names(table(d[,j], useNA=NAkeep)), "  --"))
      } else {
        tabX = data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))))
      }
    } else if (is.numeric(d[,j])){
      tabX = data.frame(paste(" "))
    }
    for (i in 1:length(levels(d$split))){
      if (is.factor(d[,j])){
        tabX = data.frame(tabX, 
                          paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1)), " (", 
                                 round(tab2[[j]][[i]]*100, 1), "%)"))
      } else if (is.numeric(d[,j])){
        tabX = data.frame(tabX, 
                          paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1, digits = 2, format = "f")), " (", 
                                 suppressWarnings(formatC(tab2[[j]][[i]], big.mark = f1, digits = 2, format = "f")), ")"))
      }
    }
    
    
    # == # Optional Odds Ratio Table # == #
    
    if (test & test_type == "or" & NAkeep == "no"){
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
    
    if (test & format_output=="full"){
      if (is.factor(d[,j])){
        n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                        paste("Chi Square:", round(tests[[j]]$statistic,2)), 
                        paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
      } else if (is.numeric(d[,j])){
        if (length(levels(d$split))>2){
          n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                          paste("F-Value:", round(tests[[j]]$statistic[[1]],2)), 
                          paste(ifelse(tests[[j]]$p.value[1] < .001, "<.001", round(tests[[j]]$p.value[1],3))))
        } else {
          n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                          paste("T-Test:", round(tests[[j]]$statistic[[1]],2)), 
                          paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
        }
      }
      tabX = data.frame(tabX, "", "")
      names(tabZ) = names(tabX) = names(n3) = c(" ", levels(d$split), "Test", "P-Value")
      tabW = rbind(n3, tabX)
      tabZ = rbind(tabZ, tabW)
      
    } else if (test & format_output=="pvalues"){
      if (is.factor(d[,j])){
        n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1),
                        paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
      } else if (is.numeric(d[,j])){
        if (length(levels(d$split))>2){
          n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                          paste(ifelse(tests[[j]]$p.value[1] < .001, "<.001", round(tests[[j]]$p.value[1],3))))
        } else {
          n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1), 
                          paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
        }
      }
      tabX = data.frame(tabX, "")
      names(tabZ) = names(tabX) = names(n3) = c(" ", levels(d$split), "P-Value")
      tabW = rbind(n3, tabX)
      tabZ = rbind(tabZ, tabW)
      
    } else if (test & format_output=="stars"){
      n3 = data.frame(names(d)[j], matrix(" ", ncol=length(levels(d$split)), nrow=1),
                      paste( ifelse(tests[[j]]$p.value < 0.001, "***", 
                             ifelse(tests[[j]]$p.value < 0.01,  "**", 
                             ifelse(tests[[j]]$p.value < 0.05,  "*", "")))))
      tabX = data.frame(tabX, "")
      names(tabZ) = names(tabX) = names(n3) = c(" ", levels(d$split), " ")
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
  
  N = suppressWarnings(formatC(N, big.mark = f1, digits = 0, format = "f"))
  
  if (format_output=="full" & test){
    N = data.frame("Observations", N, "", "")
    names(N) = c(" ", levels(d$split), "Test", "P-Value")
  } else if ((format_output=="pvalues" | format_output=="stars") & test){
    N = data.frame("Observations", N, " ") 
    if (format_output=="pvalues"){
      names(N) = c(" ", levels(d$split), "P-Value")
    } else {
      names(N) = c(" ", levels(d$split), " ")
    }
  } else {
    N = data.frame("Observations", N)
    names(N) = c(" ", levels(d$split))
  }
  
  ## Adjusting type of N
  N[] = sapply(N, as.character)
  
  ## Add formatted lines below header
  if (output_type == "text2"){
    N = rbind(N, N)
    for (i in seq_along(N)){
      N[1,i] = paste0(rep("-", times = nchar(names(N)[i])), collapse = "")
    }
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
  
  cat("Variable analyzed: ", missingvariable_)
  # === # FINAL OUTPUT # === #
  
  if (length(levels(d$split)) == 1){
    names(final)[2] = "Mean/Count (SD/%)"
  }
  
  final_l = list(final)
  
  if (grepl("text", output_type)){  ## regular text output
    class(final_l) = c("table1", "list")
    if (piping){
      print(final_l)
      invisible(.data)
    } else {
      return(final_l)
    } 
  } else if (output_type %in% c("latex", "markdown", "html", "pandoc", "rst")){ ##  output from kable
    if (piping){
      knitr::kable(final, format=output_type,
                   booktabs = booktabs,
                   caption = caption,
                   align = align,
                   row.names = FALSE)
      invisible(.data)
    } else {
      knitr::kable(final, format=output_type,
                   booktabs = booktabs,
                   caption = caption,
                   align = align,
                   row.names = FALSE)
    }
  }
}

