#' Table 1 for Simple and Stratified Descriptive Statistics
#' 
#' Produces a descriptive table, stratified by an optional categorical variable, 
#' providing means/frequencies and standard deviations/percentages. 
#' It is well-formatted for easy transition to academic article or report. 
#' Can be used within the piping framework [see library(magrittr)].
#' 
#' @param .data the data.frame that is to be summarized
#' @param ... variables in the data set that are to be summarized; unquoted names separated by commas (e.g. age, gender, race) or indices. If indices, it needs to be a single vector (e.g. c(1:5, 8, 9:20) instead of 1:5, 8, 9:20). As it is currently, it CANNOT handle both indices and unquoted names simultaneously.
#' @param splitby the categorical variable to stratify by in formula form (e.g., \code{splitby = ~gender}) or quoted (e.g., \code{splitby = "gender"}); not too surprisingly, it requires that the number of levels be > 0
#' @param FUN the function to be applied to summarize the numeric data; default is to report the means and standard deviations
#' @param FUN2 a secondary function to be applied to summarize the numeric data; default is to report the medians and 25\% and 75\% quartiles
#' @param second a vector or list of quoted continuous variables for which the \code{FUN2} should be applied
#' @param row_wise how to calculate percentages for factor variables when \code{splitby != NULL}: if \code{FALSE} calculates percentages by variable within groups; if \code{TRUE} calculates percentages across groups for one level of the factor variable.
#' @param test logical; if set to \code{TRUE} then the appropriate bivariate tests of significance are performed if splitby has more than 1 level
#' @param type what is displayed in the table; a string or a vector of strings. Two main sections can be inputted: 1. if test = TRUE, can write "pvalues", "full", or "stars" and 2. can state "simple" and/or "condense". These are discussed in more depth in the details section below.
#' @param output how the table is output; can be "text" or "text2" for regular console output or any of \code{kable()}'s options from \code{knitr} (e.g., "latex", "markdown", "pandoc").
#' @param rounding_perc the number of digits after the decimal for percentages; default is 1
#' @param var_names custom variable names to be printed in the table
#' @param format_number default in FALSE; if TRUE, then the numbers are formatted with commas (e.g., 20,000 instead of 20000)
#' @param NAkeep when set to \code{TRUE} it also shows how many missing values are in the data for each categorical variable being summarized
#' @param booktabs when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param caption when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param align when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param export character; when given, it exports the table to a CSV file to folder named "table1" in the working directory with the name of the given string (e.g., "myfile" will save to "myfile.csv")
#' 
#' @details In defining \code{type}, 1. options are "pvalues" that display the p-values of the tests, "full" which also shows the test statistics, or "stars" which only displays stars to highlight significance with *** < .001 ** .01 * .05; and
#' 2. "simple" then only percentages are shown for categorical variable and
#' "condense" then continuous variables' means and SD's will be on the same line as the variable name and dichotomous variables only show counts and percentages for the reference category.
#' 
#' @return A table with the number of observations, means/frequencies and standard deviations/percentages is returned. The object is a \code{table1} class object with a print method. Can be printed in \code{LaTex} form.
#'
#' @examples 
#' ## Ficticious Data ##
#' library(furniture)
#' library(tidyverse)
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
#'          splitby = ~a) %>%
#'   summarise(count = n())
#' 
#' ## Adjust variables within function
#' table1(df, ifelse(x > 0, 1, 0), z,
#'        var_names = c("X2", "Z"))
#'          
#'
#' @export
#' @import stats
#' @importFrom utils write.csv
#' @importFrom knitr kable
table1 = function(.data, 
                  ..., 
                  splitby = NULL, 
                  FUN = NULL,
                  FUN2 = NULL,
                  second = NULL,
                  row_wise = FALSE, 
                  test = FALSE, 
                  type = "pvalues",
                  output = "text",
                  rounding_perc = 1,
                  var_names = NULL, 
                  format_number = FALSE,
                  NAkeep = FALSE, 
                  booktabs = TRUE, 
                  caption = NULL, 
                  align = NULL,
                  export = NULL){
  
  ###################
  ## Preprocessing ##
  ###################
  .call = match.call()
  ## Type
  format_output = type[which(type %in% c("pvalue", "pvalues", "pval", "pvals", "p",
                                          "full", "f",
                                          "stars", "s"))]
  if (any(grepl("simp", type)) & any(grepl("cond", type))){
    simple = TRUE
    condense = TRUE
  } else if (any(grepl("cond", type))){
    simple = FALSE
    condense = TRUE
  } else if (any(grepl("simp", type))){
    simple = TRUE
    condense = FALSE
  } else {
    simple = FALSE
    condense = FALSE
  }
  ## Formatting for default summaries
  if (format_number){
    f1 = ","
  } else {
    f1 = ""
  }
  ## Auto-detect piping
  if (paste(.call)[[2]] == "."){
    piping = TRUE
  } else {
    piping = FALSE
  }
  ## Primary Function
  if(is.null(FUN)){
    num_fun <- function(x){
      gettextf("%s (%s)",
               formatC(mean(x, na.rm=TRUE), big.mark = f1, digits = 1, format = "f"),
               formatC(sd(x, na.rm=TRUE),   big.mark = f1, digits = 1, format = "f"))
    }
  } else {
    num_fun <- FUN
  }
  ## Secondary Function
  if(is.null(FUN2)){
    num_fun2 <- function(x){
      gettextf("%s [%s]",
               formatC(median(x, na.rm=TRUE), big.mark = f1, digits = 1, format = "f"),
               formatC(IQR(x, na.rm=TRUE),    big.mark = f1, digits = 1, format = "f"))
    }
  } else {
    num_fun2 <- FUN2
  }
  ## Missing values in categorical variables
  if (NAkeep){ 
    NAkeep = "always" 
  } else {
    NAkeep = "no"
  }
  ## Only pvalues are shown in simple or condensed versions
  if (simple | condense){
    format_output = "pvalue"
  }

  ########################
  ## Variable Selecting ##
  ########################
  ## All Variables or Selected Variables using table1_()
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
  ## For print method
  if (is.null(splitby)){
    splitting = NULL
  } else{
    splitting = paste(splitby)[[length(paste(splitby))]]
  }
  ## Splitby variable needs to have more than one level when test = TRUE
  if (test & length(levels(d$split))>1){
    test = TRUE
  } else {
    test = FALSE
  }
  
  ##################
  ## Observations ##
  ##################
  N   = t(tapply(d[[1]], d$split, length))
  N[] = sapply(N, as.character)
  N = suppressWarnings(formatC(N, big.mark = f1, digits = 0, format = "f"))
  ## Formatting the N line
  if (grepl("f|F", format_output) & test){
    N = data.frame("Observations", N, "", "")
    names(N) = c(" ", levels(d$split), "Test", "P-Value")
  } else if ((grepl("p|P", format_output) | grepl("s|S", format_output)) & test){
    N = data.frame("Observations", N, " ") 
    if (grepl("p|P", format_output)){
      names(N) = c(" ", levels(d$split), "P-Value")
    } else {
      names(N) = c(" ", levels(d$split), " ")
    }
  } else {
    N = data.frame("Observations", N)
    names(N) = c(" ", levels(d$split))
  }
  N[] = sapply(N, as.character)
  ## Add formatted lines below header
  if (output == "text2"){
    N = rbind(N, N)
    for (i in seq_along(N)){
      N[1,i] = paste0(rep("-", times = nchar(names(N)[i])), collapse = "")
    }
  }
  
  ######################
  ## Summarizing Data ##
  ######################
  summed = table1_summarizing(d, num_fun, num_fun2, second, row_wise, test, NAkeep)
  tab    = summed[[1]]
  tab2   = summed[[2]]
  tests  = summed[[3]]
  nams   = summed[[4]]
  
  ######################
  ## Formatting Table ## 
  ######################
  ## Not Condensed or Condensed
  if (!condense){
    tabZ = table1_format_nocondense(d, tab, tab2, tests, test, NAkeep, rounding_perc, 
                                    format_output, second, nams, simple, output, f1)
  } else if (condense){
    tabZ = table1_format_condense(d, tab, tab2, tests, test, NAkeep, rounding_perc, 
                                  format_output, second, nams, simple, output, f1)
  }
  ## Combine Aspects of the table
  tabZ = rbind(N, tabZ)
  rem  = ifelse(is.na(tabZ[,2]), FALSE, TRUE)
  final = tabZ[rem,]
  final$` ` = as.character(final$` `)
  
  ##################
  ## FINAL OUTPUT ##
  ##################
  if (length(levels(d$split)) == 1){
    names(final)[2] = "Mean/Count (SD/%)"
  }
  final_l = list("Table1"  = final,
                 "Splitby" = splitting)
  ## Export Option
  if (!is.null(export)){
    if (!dir.exists("Table1")){
      dir.create("Table1")
    }
    write.csv(final, file = paste0(getwd(), "/Table1/", export, ".csv"), row.names = FALSE)
  }
  
  ## regular text output
  if (grepl("text", output)){ 
    class(final_l) = c("table1", "list")
    if (piping){
      print(final_l)
      invisible(.data)
    } else {
      return(final_l)
    } 
  ## Output from kable
  } else if (output %in% c("latex", "markdown", "html", "pandoc", "rst")){
    if (piping){
      kab = knitr::kable(final, format=output,
                   booktabs = booktabs,
                   caption = caption,
                   align = align,
                   row.names = FALSE)
      print(kab)
      invisible(.data)
    } else {
      kab = knitr::kable(final, format=output,
                   booktabs = booktabs,
                   caption = caption,
                   align = align,
                   row.names = FALSE)
      return(kab)
    }
  } else {
    stop(paste("Output of type", output, "not recognized"))
  }
}


#' @export
print.table1 <- function(x, ...){
  max_col_width = max_col_width2 = list()
  ## Extract data set
  x2 = as.data.frame(x[[1]])
  
  ## Splitby Name and Location
  if (!is.null(x[[2]])){
    x3 = as.data.frame(x[[1]])
    x4 = x3[,-1]
    x5 = x3[, 1]
    x4[] = sapply(x4, as.character)
    x5[] = sapply(x5, as.character)
    for (i in 1:dim(x4)[2]){
      max_col_width2[[i]] = max(sapply(x4[[i]], nchar, type="width"))
    }
    max_col_width3 = max(sapply(x5, nchar, type="width"))
    var_width = sum(ifelse(unlist(max_col_width2) > nchar(names(x4)), unlist(max_col_width2), nchar(names(x4)))) + 
      dim(x4)[2] - 1
    first_width = sum(ifelse(unlist(max_col_width3) > nchar("Observations"), unlist(max_col_width3), nchar("Observations")))
  }

  x2[] = sapply(x2, as.character)

  ## Get width of table for lines
  for (i in 1:dim(x2)[2]){
    max_col_width[[i]] = max(sapply(x2[[i]], nchar, type="width"))
  }
  tot_width = sum(ifelse(unlist(max_col_width) > nchar(names(x2)), unlist(max_col_width), nchar(names(x2)))) + 
    dim(x2)[2] - 1
  
  ## Print top border
  cat("\n|")
  for (i in 1:tot_width){
    cat("=")
  }
  cat("|\n") 
  ## Print splitby name
  if (!is.null(x[[2]])){
    for (i in 1:round(var_width/2 + first_width - length(x[[2]])/2 - 3)){
      cat(" ")
    }
    cat(x[[2]][[1]], "\n")
  }
  ## Print table
  print(x[[1]], ..., row.names = FALSE, right = FALSE)
  ## Print bottom border
  cat("|")
  for (i in 1:tot_width){
    cat("=")
  }
  cat("|\n")
}

#' Internal Table 1 Function
#' 
#' For internal use in table1() to extract the right data.
#' 
#' @param ... the variables
#' @param d_ the data.frame
#' @param .cl the original function call
#' 
#' @return A data.frame
#'
#' @export
#' @import stats
table1_ <- function(..., d_, .cl=NULL){
  df1 = NULL
  vars = eval(substitute(alist(...)))
  
  if (length(vars) == 0){
    df2 <- d_
  } else if (length(vars) == 1){
    if(grepl("^c\\(.*\\)$", vars)){
      ## Index
      df2   <- d_[, eval(vars[[1]])]
    } else {
      df1 <- eval(vars[[1]], d_)
      df2 <- as.data.frame(df1)
      names(df2) <- paste(vars)
    }
  } else {
    ## Var Names
    for (i in seq_along(vars)){
      df1[[i]] <- eval(vars[[i]], d_)
    }
    df2 <- as.data.frame(df1)
    names(df2) <- paste(vars)
  }
  
  ## Error catching 
  if (dim(d_)[1] != length(df2[[1]])){
    stop("There is a problem with the variable names supplied.",
         call.=FALSE)
  }
  
  return(df2)
}

#' Internal Table 1 Summarizing Function
#' 
#' For internal use in table1() to summarize data.
#' 
#' @param d the data
#' @param num_fun the summarizing function
#' @param num_fun2 the second summarizing function
#' @param second the variables to which FUN2 is to be applied
#' @param row_wise the way to compute the percentages
#' @param test should significance tests be run?
#' @param NAkeep whether NA's should be shown in the output of categorical variables
#' 
#' @return A data.frame
#'
#' @export
#' @import stats
table1_summarizing = function(d, num_fun, num_fun2, second, row_wise, test, NAkeep){
  ## Summarizing The Data
  tab = tab2 = tests = tests2 = nams = list()
  for (i in 1:(dim(d)[2]-1)){
    nams[[i]] = names(d)[i]
    ## If character
    if (is.character(d[,i])){
      d[,i] = factor(d[,i])
    }
    
    ## Factor ##
    if (is.factor(d[,i])){
      tab[[i]] = tapply(d[,i], d$split, table, useNA=NAkeep)
      if (!row_wise){
        tab2[[i]] = tapply(d[,i], d$split, function(x) table(x, useNA=NAkeep)/sum(table(x, useNA=NAkeep)))
      } else if (row_wise){
        tab2[[i]] = tapply(d[,i], d$split, function(x) table(x, useNA=NAkeep)/table(d[,i], useNA=NAkeep))
      }
      if (test)
        tests[[i]] = chisq.test(d$split, d[,i])
      
    ## Numeric ##
    } else if (is.numeric(d[,i]) | is.integer(d[,i])){
      ## Function 1
      if (!nams[[i]] %in% second){
        tab[[i]]  = tapply(d[,i], d$split, num_fun)
        ## Function 2
      } else if (nams[[i]] %in% second){
        tab[[i]]  = tapply(d[,i], d$split, num_fun2)
      }
      
      ## For splitby vars with more than 2 levels
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
      
    } else {
      stop("Variables need to be either factor, character or numeric.")
    }
  }
  
  invisible(list(tab, tab2, tests, nams))
}

#' Internal Table 1 Formatting Function (No Condense)
#' 
#' For internal use in table1() to format table without condensing.
#' 
#' @param d the data
#' @param tab the summary statistics
#' @param tab2 the secondary summary statistics
#' @param tests the tests
#' @param test whether there should be significance tests performed
#' @param NAkeep logical; should we keep the NA's in factor variables
#' @param rounding_perc the number of decimal places to round the percentages
#' @param format_output the output to be displayed (see \code{table1()})
#' @param second the list of variables to apply FUN2
#' @param nams the variables to which FUN2 is to be applied
#' @param simple only percentages to be produced?
#' @param output how to print the table (see \code{table1()})
#' @param f1 the formatting of the numbers
#' 
#' @return A data.frame
#'
#' @export
table1_format_nocondense = function(d, tab, tab2, tests, test, NAkeep, rounding_perc, format_output, second, nams, simple, output, f1){
  if (test){
    if (grepl("f|F", format_output))
      tabZ = data.frame(matrix(nrow=0, ncol=length(levels(d$split))+3))
    else if (grepl("p|P", format_output) | grepl("s|S", format_output))
      tabZ = data.frame(matrix(nrow=0, ncol=length(levels(d$split))+2))
  } else {
    tabZ = data.frame(matrix(nrow=0, ncol=length(levels(d$split))+1))
  }
  
  for (j in 1:length(tab)){
    if (is.factor(d[,j])){
      if (!grepl("^t", output)){
        tabX = data.frame(paste("--  ", names(table(d[,j], useNA=NAkeep)), "  --"))
      } else {
        tabX = data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))))
      }
    } else if (is.numeric(d[,j])){
      tabX = data.frame(paste(" "))
    }
    
    ## Factor
    for (i in 1:length(levels(d$split))){
      if (is.factor(d[,j])){
        if (!simple){
          tabX = data.frame(tabX, 
                            paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1)), " (", 
                                   round(tab2[[j]][[i]]*100, rounding_perc), "%)"))
        } else if (simple){
          tabX = data.frame(tabX, 
                            paste0(round(tab2[[j]][[i]]*100, rounding_perc), "%"))
        }
        
    ## Numeric
      } else if (is.numeric(d[,j])){
        tabX = data.frame(tabX, tab[[j]][[i]])
      }
    }
    
    ## If test == TRUE, tests of comparisons by split ##
    if (test & grepl("f|F", format_output)){
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
      
    } else if (test & grepl("p|P", format_output)){
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
      
    } else if (test & grepl("s|S", format_output)){
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
  invisible(tabZ)
}

#' Internal Table 1 Formatting Function (Condense)
#' 
#' For internal use in table1() to format table with condensing.
#' 
#' @param d the data
#' @param tab the summary statistics
#' @param tab2 the secondary summary statistics
#' @param tests the tests
#' @param test whether there should be significance tests performed
#' @param NAkeep logical; should we keep the NA's in factor variables
#' @param rounding_perc the number of decimal places to round the percentages
#' @param format_output the output to be displayed (see \code{table1()})
#' @param second the list of variables to apply FUN2
#' @param nams the variables to which FUN2 is to be applied
#' @param simple only percentages to be produced?
#' @param output how to print the table (see \code{table1()})
#' @param f1 the formatting of the numbers
#' 
#' @return A data.frame
#'
#' @export
table1_format_condense = function(d, tab, tab2, tests, test, NAkeep, rounding_perc, format_output, second, nams, simple, output, f1){
  if (test){
    if (grepl("p|P", format_output) | grepl("s|S", format_output))
      tabZ = data.frame(matrix(nrow=0, ncol=length(levels(d$split))+2))
  } else {
    tabZ = data.frame(matrix(nrow=0, ncol=length(levels(d$split))+1))
  }
  
  for (j in 1:length(tab)){
    if (is.factor(d[,j])){
      if (length(levels(d[,j])) == 2){
        if (!grepl("text", output)){
          tabX = data.frame(paste0(names(d)[j], ": ", names(table(d[,j], useNA=NAkeep)[2])))
        } else {
          tabX = data.frame(paste0(names(d)[j], ": ", names(table(d[,j], useNA=NAkeep)[2])))
        }
      } else if (length(levels(d[,j])) > 2){
        if (!grepl("text", output)){
          tabX = data.frame(paste("--  ", names(table(d[,j], useNA=NAkeep))))
        } else {
          tabX = data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))))
        }
      }
    } else if (is.numeric(d[,j])){
      tabX = data.frame(paste(names(d)[j]))
    }
    
    ## Counts and Percentages or Just Percentages
    for (i in 1:length(levels(d$split))){
      if (is.factor(d[,j])){
        ## Just percentages
        if (simple){
          if (length(levels(d[,j])) == 2){
            tabX = data.frame(tabX, 
                              paste0(round(tab2[[j]][[i]][2]*100, 1), "%"))
          } else {
            tabX = data.frame(tabX, 
                              paste0(round(tab2[[j]][[i]]*100, 1), "%"))
          }
          ## Counts and Percentages
        } else {
          if (length(levels(d[,j])) == 2){
            tabX = data.frame(tabX, 
                              paste0(suppressWarnings(formatC(tab[[j]][[i]][2], big.mark = f1)), " (", 
                                     round(tab2[[j]][[i]][2]*100, 1), "%)"))
          } else {
            tabX = data.frame(tabX, 
                              paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1)), " (", 
                                     round(tab2[[j]][[i]]*100, 1), "%)"))
          }
        }
        
        
      } else if (is.numeric(d[,j])){
        tabX = data.frame(tabX, tab[[j]][[i]])
      }
    }
    
    ## If test == TRUE, tests of comparisons by split ##
    if (test){
      if (is.factor(d[,j])){
        if (length(levels(d[,j])) == 2){
          n3 = data.frame(tabX, 
                          paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
          tabX = n3
        } else {
          blankX = data.frame(names(d)[j], 
                              matrix(" ", ncol=(length(levels(d$split))), nrow = 1),
                              paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
          n3 = data.frame(tabX, " ")
          names(blankX) = names(n3)
          tabX = rbind(blankX, n3)
        }
      } else if (is.numeric(d[,j])){
        if (length(levels(d$split))>2){
          n3 = data.frame(tabX, 
                          paste(ifelse(tests[[j]]$p.value[1] < .001, "<.001", round(tests[[j]]$p.value[1],3))))
          tabX = n3
        } else {
          n3 = data.frame(tabX, 
                          paste(ifelse(tests[[j]]$p.value < .001, "<.001", round(tests[[j]]$p.value,3))))
          tabX = n3
        }
      }
      names(tabZ) = names(tabX) = c(" ", levels(d$split), "P-Value")
      tabZ = rbind(tabZ, tabX)
      
    } else if (!test){
      if (is.factor(d[,j])){
        if (length(levels(d[,j])) == 2){
          ## Nothing
        } else {
          blankX = data.frame(names(d)[j], matrix(" ", ncol=(length(levels(d$split))), nrow = 1))
          n3 = data.frame(tabX)
          names(blankX) = names(n3)
          tabX = rbind(blankX, n3)
        }
      } else if (is.numeric(d[,j])){
        if (length(levels(d$split))>2){
          ## Nothing
        } else {
          ## Nothing
        }
      }
      names(tabZ) = names(tabX) = c(" ", levels(d$split))
      tabZ = rbind(tabZ, tabX)
    }
  }
  invisible(tabZ)
}

