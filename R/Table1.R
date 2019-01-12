#' Table 1 for Simple and Stratified Descriptive Statistics
#' 
#' Produces a descriptive table, stratified by an optional categorical variable, 
#' providing means/frequencies and standard deviations/percentages. 
#' It is well-formatted for easy transition to academic article or report. 
#' Can be used within the piping framework [see library(magrittr)].
#' 
#' @param .data the data.frame that is to be summarized
#' @param ... variables in the data set that are to be summarized; unquoted names separated by commas (e.g. age, gender, race) or indices. If indices, it needs to be a single vector (e.g. c(1:5, 8, 9:20) instead of 1:5, 8, 9:20). As it is currently, it CANNOT handle both indices and unquoted names simultaneously. Finally, any empty rows (where the row is NA for each variable selected) will be removed for an accurate n count.
#' @param splitby the categorical variable to stratify (in formula form \code{splitby = ~gender}) or quoted \code{splitby = "gender"}; instead, \code{dplyr::group_by(...)} can be used within a pipe (this is the default when the data object is a grouped data frame from \code{dplyr::group_by(...)}).
#' @param FUN the function to be applied to summarize the numeric data; default is to report the means and standard deviations
#' @param FUN2 a secondary function to be applied to summarize the numeric data; default is to report the medians and 25\% and 75\% quartiles
#' @param total whether a total (not stratified with the \code{splitby} or \code{group_by()}) should also be reported in the table
#' @param second a vector or list of quoted continuous variables for which the \code{FUN2} should be applied
#' @param row_wise how to calculate percentages for factor variables when \code{splitby != NULL}: if \code{FALSE} calculates percentages by variable within groups; if \code{TRUE} calculates percentages across groups for one level of the factor variable.
#' @param test logical; if set to \code{TRUE} then the appropriate bivariate tests of significance are performed if splitby has more than 1 level. A message is printed when the variances of the continuous variables being tested do not meet the assumption of Homogeneity of Variance (using Breusch-Pagan Test of Heteroskedasticity) and, therefore, the argument `var.equal = FALSE` is used in the test.
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
#' @param float the float applied to the table in Latex when output is \code{latex2}, default is "ht".
#' @param export character; when given, it exports the table to a CSV file to folder named "table1" in the working directory with the name of the given string (e.g., "myfile" will save to "myfile.csv")
#' @param label for \code{output == "latex2"}, this provides a table reference label for latex
#' 
#' @details In defining \code{type}, 1. options are "pvalues" that display the p-values of the tests, "full" which also shows the test statistics, or "stars" which only displays stars to highlight significance with *** < .001 ** .01 * .05; and
#' 2. "simple" then only percentages are shown for categorical variable and
#' "condense" then continuous variables' means and SD's will be on the same line as the variable name and dichotomous variables only show counts and percentages for the reference category.
#' 
#' @return A table with the number of observations, means/frequencies and standard deviations/percentages is returned. The object is a \code{table1} class object with a print method. Can be printed in \code{LaTex} form.
#'
#' @examples 
#' 
#' ## Fictitious Data ##
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
#' ## all three below are the same
#' table1(df, x, y, z,
#'        splitby = ~ a)
#' table1(df, x, y, z,
#'        splitby = "a")
#' 
#' ## With Piping
#' df %>%
#'   table1(x, y, z, 
#'          splitby = ~a) 
#'          
#' df %>%
#'   group_by(a) %>%
#'   table1(x, y, z)
#' 
#' ## Adjust variables within function and assign name
#' table1(df, 
#'        x2 = ifelse(x > 0, 1, 0), z = z)
#'
#' @export
#' @importFrom stats IQR addmargins complete.cases dchisq lm median model.frame oneway.test pt resid sd setNames t.test
#' @importFrom utils write.csv
#' @importFrom knitr kable
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
table1 = function(.data, 
                   ..., 
                   splitby = NULL, 
                   FUN = NULL,
                   FUN2 = NULL,
                   total = FALSE,
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
                   export = NULL,
                   label = NULL){
  UseMethod("table1", .data)
}


#' @export
#' @importFrom utils write.csv
#' @importFrom knitr kable
#' @importFrom forcats fct_drop
#' @importFrom dplyr filter
table1.data.frame = function(.data, 
                  ..., 
                  splitby = NULL, 
                  FUN = NULL,
                  FUN2 = NULL,
                  total = FALSE,
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
                  export = NULL,
                  label = NULL){
  
  ###################
  ## Preprocessing ##
  ###################
  .call <- match.call()
  ## Test output
  format_output <- type[which(type %in% c("pvalue", "pvalues", "pval", "pvals", "p",
                                          "full", "f",
                                          "stars", "s"))]
  ## Table type
  cond_simp <- .type_constructor(type)
  condense  <- cond_simp[[1]]
  simple    <- cond_simp[[2]]
  
  ## checks
  .header_labels(header_labels, format_output)
  
  ## Deprecation (drop in furniture 2.0.0)
  if (!is.null(NAkeep)){
    warning("NAkeep is deprecated. Please use na.rm instead.\nNote that {NAkeep = TRUE} == {na.rm = FALSE}.", 
            call. = FALSE)
    na.rm <- !NAkeep
  }
  ## Not yet deprecated
  #if (!is.null(splitby))
  #  warning("`splitby` is deprecated. Use dplyr::group_by() instead. It's use will continue until furniture 2.0.0")
  
  ## Missing values in categorical variables
  if (isTRUE(na.rm)){ 
    NAkeep <- "no" 
  } else {
    NAkeep <- "always"
  }
  ## Only pvalues are shown in simple or condensed versions
  if (simple | condense){
    format_output <- "pvalue"
  }
  ## Formatting default functions
  if (format_number){
    f1 <- ","
  } else {
    f1 <- ""
  }
  ## Functions
  num_fun  <- .summary_functions1(FUN, format_number, digits)
  num_fun2 <- .summary_functions2(FUN2, format_number, digits)

  ########################
  ## Variable Selecting ##
  ########################
  ## All Variables or Selected Variables using selecting()
  d <- selecting(data.frame(.data), ...) %>%
    setNames(gsub("\\.", " ", names(.)))
  
  ### Naming of variables
  if (!is.null(var_names)){
    stopifnot(length(var_names)==length(names(d)))
    names(d) <- var_names
  }
  
  ## Splitby or group_by
  if (is.null(attr(.data, "vars")) && is.null(attr(.data, "groups"))){
    
    ### Splitby Variable (adds the variable to d as "split")
    if (!is.null(splitby))
      splitby <- substitute(splitby)
    if (class(substitute(splitby)) == "name"){
      splitby_ <- eval(substitute(splitby), .data)
    } else if (class(substitute(splitby)) == "call"){
      splitby_ <- model.frame(splitby, .data, na.action = "na.pass")[[1]]
    } else if (class(substitute(splitby)) == "character"){
      splitby_ <- .data[[splitby]]
    } else if(is.null(splitby)){
      splitby_ <- factor(1)
    }
    d$split = factor(splitby_)
    ## For print method
    if (is.null(splitby)){
      splitting <- NULL
    } else {
      splitting <- paste(splitby)[[length(paste(splitby))]]
    }
    ## Remove any redundant grouping vars
    if (length(which(names(d) %in% splitby_)) != 0){
      d <- d[, -which(names(d) %in% splitby_), drop = FALSE]
    }

  } else {
    
    ## Working around different versions of dplyr with group_by()
    ## Older (0.7.6) uses "vars": produces the grouping name
    ## Developmental one (0.7.9.9000) uses "groups" but it produces a nested table
    groups <- attr(.data, "vars")
    if (is.null(groups))
      groups <- attr(.data, "groups") %>% names(.)
    if (groups[length(groups)] == ".rows")
      groups <- groups[-length(groups)]
    
    message(paste0("Using dplyr::group_by() groups: ", paste(groups, collapse = ", ")))
    
    if (length(groups) == 1){
      d$split <- factor(.data[[groups]])
    } else {
      interacts <- interaction(.data[groups], sep = "_")
      d$split <- factor(interacts)
    }
    ## For print method
    if (is.null(groups)){
      splitting <- NULL
    } else{
      splitting <- paste(groups, collapse = ", ")
    }
    ## Remove any redundant grouping vars
    if (length(which(names(d) %in% groups)) != 0){
      d <- d[, -which(names(d) %in% groups), drop = FALSE]
    }
  }

  
  ## Remove missing values?
  if (isTRUE(na.rm))
    d <- d[complete.cases(d), , drop = FALSE]
  if (nrow(d) == 0)
    stop("No non-missing values in data frame with `na.rm = TRUE`", call. = FALSE)

  
  ## Splitby variable needs to have more than one level when test = TRUE
  if (test & length(levels(d$split))>1){
    test <- TRUE
  } else {
    test <- FALSE
  }
  
  ## Does each variable have at least two levels?
  if (! .more_than_one_value(d)){
    warning(paste("Not all variables have at least 2 unique values.",
                  "Functionality of the following will be limited:\n",
                  " -- `type = 'condense'` will not work\n",
                  " -- `test = TRUE` will not work"), 
            call. = FALSE)
  }
  
  if (isTRUE(total & test)){
    message("The test is for the stratified data relationships.")
  }
  
  if (isTRUE(levels(d$split) == 1 & total)){
    total = FALSE
  }
  
  ####################################
  ## Observations and Header Labels ##
  ####################################
  N <- .obs_header(d, f1, format_output, test, output, header_labels, total)

  ######################
  ## Summarizing Data ##
  ######################
  summed <- table1_summarizing(d, num_fun, num_fun2, second, row_wise, test, NAkeep, total)
  tab    <- summed[[1]]
  tab2   <- summed[[2]]
  tests  <- summed[[3]]
  nams   <- summed[[4]]
  
  ######################
  ## Formatting Table ## 
  ######################
  ## Not Condensed or Condensed
  if (!condense){
    tabZ <- table1_format_nocondense(d, tab, tab2, tests, test, NAkeep, rounding_perc, 
                                     format_output, second, nams, simple, output, f1, total)
  } else if (condense){
    tabZ <- table1_format_condense(d, tab, tab2, tests, test, NAkeep, rounding_perc, 
                                   format_output, second, nams, simple, output, f1, total)
  }
  ## Combine Aspects of the table
  names(tabZ) <- names(N)
  tabZ <- rbind(N, tabZ)
  rem <- ifelse(is.na(tabZ[,2]), FALSE, TRUE)
  final <- tabZ[rem,]
  final$` ` <- as.character(final$` `)
  
  ##################
  ## FINAL OUTPUT ##
  ##################
  if (length(levels(d$split)) == 1){
    names(final)[2] <- "Mean/Count (SD/%)"
  }
  final_l <- list("Table1" = final)
  attr(final_l, "splitby") <- splitting
  attr(final_l, "output") <- output
  attr(final_l, "tested") <- test
  attr(final_l, "total") <- total
  
  ## Export Option
  if (!is.null(export)){
    if (!dir.exists("Table1")){
      dir.create("Table1")
    }
    write.csv(final, file = paste0(getwd(), "/Table1/", export, ".csv"), row.names = FALSE)
  }
  
  ## regular text output
  if (grepl("text", output)){ 
    class(final_l) <- c("table1")
    cat("\n", caption)
    return(final_l)

  ## Custom Latex Output
  } else if (output %in% "latex2"){
    if (is.null(align)){
      l1 <- dim(final)[2]
      align <- c("l", rep("c", (l1-1)))
    }
    tab <- to_latex(final, caption, align, len = length(levels(d$split)), splitting, float, booktabs, label)
    tab
  ## Output from kable  
  } else if (output %in% c("latex", "markdown", "html", "pandoc", "rst")){
    kab <- knitr::kable(final, format=output,
                 booktabs = booktabs,
                 caption = caption,
                 align = align,
                 row.names = FALSE)
    return(kab)
  } else {
    stop(paste("Output of type", output, "not recognized"))
  }
}



#' @export
print.table1 <- function(x, ...){
  max_col_width = max_col_width2 = max_col_width3 = list()
  ## Extract data set
  x2 <- as.data.frame(x[[1]])
  
  if (isTRUE(attr(x, "total"))){
    first_part <- c(1,2)
  } else {
    first_part <- c(1)
  }
  
  if (isTRUE(attr(x, "tested"))){
    last_part <- ncol(x2)
  } else {
    last_part <- NULL
  }
  
  x2[] <- sapply(x2, as.character)
  
  ## Get width of table for lines
  for (i in 1:dim(x2)[2]){
    max_col_width[[i]] <- max(sapply(x2[[i]], nchar, type="width"))
  }
  tot_width <- sum(ifelse(unlist(max_col_width) > nchar(names(x2)), unlist(max_col_width), nchar(names(x2)))) + 
    dim(x2)[2] - 1
  
  if (isTRUE(attr(x, "test")))
    max_col_width[[length(max_col_width)]] <- 7
  
  
  ## Splitby Name and Location
  if (!is.null(attr(x, "splitby"))){
    x3 <- as.data.frame(x[[1]])
    x4 <- x3[, -c(first_part, last_part), drop = FALSE]
    x5 <- x3[, first_part, drop = FALSE]
    x4[] <- sapply(x4, as.character)
    x5[] <- sapply(x5, as.character)
    for (i in 1:ncol(x4)){
      max_col_width2[[i]] <- max(sapply(x4[[i]], nchar, type="width"))
    }
    for (i in 1:ncol(x5)){
      max_col_width3[[i]] <- max(sapply(x5[[i]], nchar, type="width"))
    }
    var_width <- sum(ifelse(unlist(max_col_width2) > nchar(names(x4)), unlist(max_col_width2), nchar(names(x4)))) + 
      dim(x4)[2] - 1
    first_width <- sum(ifelse(unlist(max_col_width3) > nchar("  "), unlist(max_col_width3), nchar("  ")))
  }

  
  ## Print top border
  cat("\n\u2500")
  for (i in 1:tot_width){
    cat("\u2500")
  }
  cat("\u2500\n") 
  ## Print splitby name
  if (!is.null(attr(x, "splitby"))){
    len1 <- nchar(gsub("`", "", attr(x, "splitby")))
    for (i in 1:round(first_width + var_width/2 - len1/2)){
      cat(" ")
    }
    cat(gsub("`", "", attr(x, "splitby")), "\n")
  }
  ## Print table
  if (!is.null(attr(x, "output"))){
    if (attr(x, "output") == "text2"){
      ## Special "text2" formatting
      x4 <- rbind(x[[1]][1,],
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

#' @export
as.data.frame.table1 <- function(x, row.names = NULL, optional = FALSE, ...,
                                 cut.names = FALSE, col.names = names(x), fix.empty.names = TRUE,
                                 stringsAsFactors = default.stringsAsFactors()){
  
  as.data.frame.list(x) %>%
    setNames(., gsub("Table1\\.", "", names(.)))
  
}
