#' Table 1 for Simple and Stratified Descriptive Statistics
#' 
#' Produces a descriptive table, stratified by an optional categorical variable, 
#' providing means/frequencies and standard deviations/percentages. 
#' It is well-formatted for easy transition to academic article or report. 
#' Can be used within the piping framework [see library(magrittr)].
#' 
#' @param .data the data.frame that is to be summarized
#' @param ... variables in the data set that are to be summarized; unquoted names separated by commas (e.g. age, gender, race) or indices. If indices, it needs to be a single vector (e.g. c(1:5, 8, 9:20) instead of 1:5, 8, 9:20). As it is currently, it CANNOT handle both indices and unquoted names simultaneously.
#' @param splitby the categorical variable to stratify by in formula form (e.g., \code{splitby = ~gender}); not too surprisingly, it requires that the number of levels be > 0
#' @param splitby_labels allows for custom labels of the splitby levels; must match the number of levels of the splitby variable
#' @param test logical; if set to \code{TRUE} then the appropriate bivariate tests of significance are performed if splitby has more than 1 level
#' @param test_type has two options: "default" performs the default tests of significance only; "or" also give unadjusted odds ratios as well based on logistic regression (only use if splitby has 2 levels)
#' @param piping if \code{TRUE} then the table is printed and the original data is passed on. It is very useful in piping situations where one wants the table but wants it to be part of a larger pipe.
#' @param rounding the number of digits after the decimal; default is 3
#' @param var_names custom variable names to be printed in the table
#' @param format_output has three options: 1) "full" provides the table with the type of test, test statistic, and the p-value for each variable; 2) "pvalues" provides the table with the p-values; and 3) "stars" provides the table with stars indicating significance
#' @param output_type default is "text"; the other options are all format options in the \code{kable()} function in \code{knitr} (e.g., latex, html, markdown, pandoc)
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
#' library(dplyr)
#' 
#' x  <- runif(1000)
#' y  <- rnorm(1000)
#' z  <- factor(sample(c(0,1), 1000, replace=TRUE))
#' a  <- factor(sample(c(1,2), 1000, replace=TRUE))
#' df <- data.frame(x, y, z, a)
#' 
#' ## Simple
#' table1(df, x, y, z, a)
#' 
#' ## Stratified
#' ## both below are the same
#' table1(df, x, y, z,
#'        splitby = ~ a)
#' table1(df, x, y, z,
#'        splitby = "a")
#' 
#' ## With Piping
#' df %>%
#'   table1(x, y, z, 
#'          splitby = ~a, 
#'          piping = TRUE) %>%
#'   summarise(count = n())
#' 
#' ## Adjust variables within function
#' table1(df, ifelse(x > 0, 1, 0), z,
#'        var_names = c("Dich X", "Z"))
#'          
#'
#' @export
#' @import stats
#' @importFrom knitr kable
table1 = function(.data, ..., splitby = NULL, splitby_labels = NULL, test = FALSE, test_type = "default", piping = FALSE,
                  rounding = 3, var_names = NULL, format_output = "pvalues", output_type = "text", NAkeep = FALSE, m_label = "Missing",
                  booktabs = TRUE, caption=NULL, align=NULL){
  
  # == # Checks and Data # == #
  .call = match.call()
  
  if (NAkeep){ 
    NAkeep = "always" 
  } else {
    NAkeep = "no"
  }
  
  data = table1_(..., d_=.data, .cl=.call)
  d = as.data.frame(data)
  
  ### Naming of variables
  if (!is.null(var_names)){
    stopifnot(length(var_names)==length(names(d)))
    names(d) = var_names
  }
  
  ### Splitby Variable (adds the variable to d as "split")
  if (is.null(splitby)){
    splitby_ = as.factor(1)
    d$split  = droplevels(splitby_)
  } else {
    splitby_ = eval(parse(text = paste(splitby)[[length(paste(splitby))]]), .data)
    d$split  = droplevels(as.factor(splitby_))
  }
  
  ## Test = TRUE and the splitting variable needs to have more than one level
  if (test & length(levels(d$split))>1){
    test = TRUE
  } else {
    test = FALSE
  }
  
  if (!is.null(splitby_labels))
    levels(d$split) = splitby_labels
  
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
      tab2[[i]] = tapply(d[,i], d$split, function(x) round(table(x, useNA=NAkeep)/sum(table(x, useNA=NAkeep)), rounding))
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
        r2     = summary(lm(resids ~ nhanes$gender[comp]))$r.squared
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
      if (output_type != "text"){
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
                          paste0(round(tab[[j]][[i]],2), " (", round(tab2[[j]][[i]]*100,1), "%)"))
      } else if (is.numeric(d[,j])){
        tabX = data.frame(tabX, 
                          paste0(round(tab[[j]][[i]],2), " (", round(tab2[[j]][[i]],2), ")"))
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
  
  if (output_type == "text"){  ## regular text output
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


#' @export
print.table1 <- function(x, ...){
  x2 = as.data.frame(x[[1]])
  summed = list()
  for (i in seq_along(x2)){
    summed[[i]] = max(nchar(as.character(x2[,i]), type="width"))
  }
  w = sum(unlist(summed))
  cat("\n|=====")
  for (i in 1:w){
    cat("=")
  }
  cat("=====\n") 
  print(x[[1]], ..., row.names = FALSE, right = FALSE)
  cat("|=====")
  for (i in 1:w){
    cat("=")
  }
  cat("=====\n") 
}

#' Internal Table 1 Function
#' 
#' For internal use in table1().
#' 
#' @param ... the variables
#' @param d_ the data.frame
#' @param .cl the original functon call
#' 
#' @return A data.frame
#'
#' @export
#' @import stats
table1_ <- function(..., d_, .cl=NULL){
  df1 = df2 = NULL
  vars = eval(substitute(alist(...)))
  
  ## for dots_capture
  for (i in seq_along(vars)){
    df1[[i]] <- eval(vars[[i]], d_)
    
    ## if is an index (built on assumption that lengths will differ)
    if (length(df1[[i]]) != length(d_[[1]]) & is.numeric(df1[[i]])){
      df2 <- d_[, df1[[i]]]
    }
  }
  
  if (is.null(df2)){
    df2 <- as.data.frame(df1)
    names(df2) = paste(.cl)[3:(length(vars)+2)]
  }

  ## Error catching 
  if (dim(d_)[1] != length(df2[[1]])){
    stop("There is a problem with the variable names supplied. Make sure the ... only includes unquoted var names [e.g. gender, age] or a single vector of indices [e.g. c(3:5, 6)] or that splitby variable is stated as a formula [e.g. splitby = ~var1]",
         call.=FALSE)
  }
  
  return(df2)
}

