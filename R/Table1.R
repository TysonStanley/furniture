#' Table 1 for Simple and Stratified Descriptive Statistics
#' 
#' Produces a descriptive table, stratified by an optional categorical variable, 
#' providing means/frequencies and standard deviations/percentages. 
#' It is well-formatted for easy transition to academic article or report. 
#' Can be used within the piping framework [see library(magrittr)].
#' 
#' @param .data the data.frame that is to be summarized
#' @param ... variables in the data set that are to be summarized; unquoted names separated by commas (e.g. age, gender, race) or indices. If indices, it needs to be a single vector (e.g. c(1:5, 8, 9:20) instead of 1:5, 8, 9:20). As it is currently, it CANNOT handle both indices and unquoted names simultaneously.
#' @param splitby the categorical variable to stratify (in formula form  \code{splitby = ~gender}), quoted \code{splitby = "gender"}, or bare \code{splitby = gender}); instead, \code{dplyr::group_by(...)} can be used
#' @param FUN the function to be applied to summarize the numeric data; default is to report the means and standard deviations
#' @param FUN2 a secondary function to be applied to summarize the numeric data; default is to report the medians and 25\% and 75\% quartiles
#' @param second a vector or list of quoted continuous variables for which the \code{FUN2} should be applied
#' @param row_wise how to calculate percentages for factor variables when \code{splitby != NULL}: if \code{FALSE} calculates percentages by variable within groups; if \code{TRUE} calculates percentages across groups for one level of the factor variable.
#' @param test logical; if set to \code{TRUE} then the appropriate bivariate tests of significance are performed if splitby has more than 1 level
#' @param header_labels a character vector that renames the header labels (e.g., the blank above the variables, the p-value label, and test value label).
#' @param type what is displayed in the table; a string or a vector of strings. Two main sections can be inputted: 1. if test = TRUE, can write "pvalues", "full", or "stars" and 2. can state "simple" and/or "condense". These are discussed in more depth in the details section below.
#' @param output how the table is output; can be "text" or "text2" for regular console output or any of \code{kable()}'s options from \code{knitr} (e.g., "latex", "markdown", "pandoc"). A new option, \code{'latex2'}, although more limited, allows the variable name to show and has an overall better appearance.
#' @param rounding_perc the number of digits after the decimal for percentages; default is 1
#' @param digits the number of significant digits for the numerical variables (if using default functions); default is 1.
#' @param var_names custom variable names to be printed in the table. Variable names can be applied directly in the list of variables.
#' @param format_number default is FALSE; if TRUE, then the numbers are formatted with commas (e.g., 20,000 instead of 20000)
#' @param NAkeep when set to \code{TRUE} it also shows how many missing values are in the data for each categorical variable being summarized (deprecated; use \code{na.rm})
#' @param na.rm when set to \code{FALSE} it also shows how many missing values are in the data for each categorical variable being summarized
#' @param booktabs when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param caption when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param align when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param float the float applied to the table in Latex when output is \code{latex2}
#' @param export character; when given, it exports the table to a CSV file to folder named "table1" in the working directory with the name of the given string (e.g., "myfile" will save to "myfile.csv")
#' 
#' @details In defining \code{type}, 1. options are "pvalues" that display the p-values of the tests, "full" which also shows the test statistics, or "stars" which only displays stars to highlight significance with *** < .001 ** .01 * .05; and
#' 2. "simple" then only percentages are shown for categorical variable and
#' "condense" then continuous variables' means and SD's will be on the same line as the variable name and dichotomous variables only show counts and percentages for the reference category.
#' 
#' @return A table with the number of observations, means/frequencies and standard deviations/percentages is returned. The object is a \code{table1} class object with a print method. Can be printed in \code{LaTex} form.
#'
#' @examples 
#' 
#' \dontrun{
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
#' ## all three below are the same
#' table1(df, x, y, z,
#'        splitby = ~ a)
#' table1(df, x, y, z,
#'        splitby = "a")
#' table1(df, x, y, z,
#'        splitby = a)
#' 
#' ## With Piping
#' df %>%
#'   table1(x, y, z, 
#'          splitby = ~a) %>%
#'   summarise(count = n())
#' df %>%
#'   group_by(a) %>%
#'   table1(x, y, z)
#' 
#' ## Adjust variables within function and assign name
#' table1(df, 
#'        x2 = ifelse(x > 0, 1, 0), z = z)
#' }        
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
                   header_labels = NULL,
                   type = "pvalues",
                   output = "text",
                   rounding_perc = 1,
                   digits = 1,
                   var_names = NULL, 
                   format_number = FALSE,
                   NAkeep = NULL, 
                   na.rm = TRUE,
                   booktabs = TRUE, 
                   caption = NULL, 
                   align = NULL,
                   float = "ht",
                   export = NULL){
  UseMethod("table1", .data)
}


#' @export
#' @import stats
#' @importFrom utils write.csv
#' @importFrom knitr kable
table1.data.frame = function(.data, 
                  ..., 
                  splitby = NULL, 
                  FUN = NULL,
                  FUN2 = NULL,
                  second = NULL,
                  row_wise = FALSE, 
                  test = FALSE, 
                  header_labels = NULL,
                  type = "pvalues",
                  output = "text",
                  rounding_perc = 1,
                  digits = 1,
                  var_names = NULL, 
                  format_number = FALSE,
                  NAkeep = NULL, 
                  na.rm = TRUE,
                  booktabs = TRUE, 
                  caption = NULL, 
                  align = NULL,
                  float = "ht",
                  export = NULL){
  
  ###################
  ## Preprocessing ##
  ###################
  .call = match.call()
  ## Test output
  format_output = type[which(type %in% c("pvalue", "pvalues", "pval", "pvals", "p",
                                          "full", "f",
                                          "stars", "s"))]
  ## Table type
  cond_simp = .type_constructor(type)
  condense  = cond_simp[[1]]
  simple    = cond_simp[[2]]
  
  ## checks
  .header_labels(header_labels, format_output)
  
  ##
  if (!is.null(NAkeep)){
    warning("NAkeep is deprecated. Please use na.rm instead.\nNote that NAkeep = TRUE == na.rm = FALSE.")
    na.rm = !NAkeep
  }
  
  ## Auto-detect piping
  if (paste(.call)[[2]] == "."){
    piping = TRUE
  } else {
    piping = FALSE
  }
  ## Missing values in categorical variables
  if (!na.rm){ 
    NAkeep = "always" 
  } else {
    NAkeep = "no"
  }
  ## Only pvalues are shown in simple or condensed versions
  if (simple | condense){
    format_output = "pvalue"
  }
  ## Formatting default functions
  if (format_number){
    f1 = ","
  } else {
    f1 = ""
  }
  ## Functions
  num_fun  = .summary_functions1(FUN, format_number, digits)
  num_fun2 = .summary_functions2(FUN2, format_number, digits)

  ########################
  ## Variable Selecting ##
  ########################
  ## All Variables or Selected Variables using table1_()
  d = selecting(d_=.data, ...)
  
  
  ### Naming of variables
  if (!is.null(var_names)){
    stopifnot(length(var_names)==length(names(d)))
    names(d) = var_names
  }
  
  ## Splitby or group_by
  if (is.null(attr(.data, "vars"))){
    ### Splitby Variable (adds the variable to d as "split")
    splitby = substitute(splitby)
    if (class(substitute(splitby)) == "name"){
      splitby_ = eval(substitute(splitby), .data)
    } else if (class(substitute(splitby)) == "call"){
      splitby_ = model.frame(splitby, .data, na.action = "na.pass")[[1]]
    } else if (class(substitute(splitby)) == "character"){
      splitby_ = .data[[splitby]]
    } else if(is.null(splitby)){
      splitby_ = factor(1)
    }
    d$split = factor(splitby_)
    ## For print method
    if (is.null(splitby)){
      splitting = NULL
    } else{
      splitting = paste(splitby)[[length(paste(splitby))]]
    }
  } else {
    message("Using a grouped data frame: default using the grouping variables and not splitby")
    if (length(attr(.data, "vars")) == 1){
      d$split = droplevels(as.factor(.data[attr(.data, "vars")][[1]]))
    } else {
      interacts = interaction(.data[attr(.data, "vars")], sep = "_")
      d$split = factor(interacts)
    }
    ## For print method
    if (is.null(attr(.data, "vars"))){
      splitting = NULL
    } else{
      splitting = paste(attr(.data, "vars"), collapse = ", ")
    }
  }
  ## Splitby variable needs to have more than one level when test = TRUE
  if (test & length(levels(d$split))>1){
    test = TRUE
  } else {
    test = FALSE
  }
  
  ####################################
  ## Observations and Header Labels ##
  ####################################
  N = .obs_header(d, f1, format_output, test, output, header_labels)
  
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
  names(tabZ) = names(N)
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
  final_l = list("Table1"  = final)
  attr(final_l, "splitby") = splitting
  attr(final_l, "output") = output
  
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
      cat("\n", caption)
      return(final_l)
    } 
  ## Custom Latex Output
  } else if (output %in% "latex2"){
    if (is.null(align)){
      l1 = dim(final)[2]
      align = c("l", rep("c", (l1-1)))
    }
    tab = to_latex(final, caption, align, len = length(levels(d$split)), splitby, float, booktabs)
    tab
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
  if (!is.null(attr(x, "splitby"))){
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
    first_width = sum(ifelse(unlist(max_col_width3) > nchar("  "), unlist(max_col_width3), nchar("  ")))
  }

  x2[] = sapply(x2, as.character)

  ## Get width of table for lines
  for (i in 1:dim(x2)[2]){
    max_col_width[[i]] = max(sapply(x2[[i]], nchar, type="width"))
  }
  tot_width = sum(ifelse(unlist(max_col_width) > nchar(names(x2)), unlist(max_col_width), nchar(names(x2)))) + 
    dim(x2)[2] - 1
  
  ## Print top border
  cat("\n\u2500")
  for (i in 1:tot_width){
    cat("\u2500")
  }
  cat("\u2500\n") 
  ## Print splitby name
  if (!is.null(attr(x, "splitby"))){
    len1 = nchar(gsub("`", "", attr(x, "splitby")))
    for (i in 1:round(var_width/2 + first_width - len1/2)){
      cat(" ")
    }
    cat(gsub("`", "", attr(x, "splitby")), "\n")
  }
  ## Print table
  if (!is.null(attr(x, "output"))){
    if (attr(x, "output") == "text2"){
      ## Special "text2" formatting
      x4 = rbind(x[[1]][1,],
                 sapply(max_col_width, function(x) paste0(rep("-", times = x), collapse = "")),
                 x[[1]][2:dim(x[[1]])[1], ])
      print(x4, ..., row.names = FALSE, right = FALSE)
    } else {
      print(x[[1]], ..., row.names = FALSE, right = FALSE)
    }
  } else {
    print(x[[1]], ..., row.names = FALSE, right = FALSE)
  }
  
  ## Print bottom border
  cat("\u2500")
  for (i in 1:tot_width){
    cat("\u2500")
  }
  cat("\u2500\n")
}


