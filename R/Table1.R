#' Table 1 for Simple and Stratified Descriptive Statistics
#' 
#' Produces a descriptive table, stratified by an optional categorical variable, 
#' providing means/frequencies and standard deviations/percentages. 
#' It is well-formatted for easy transition to academic article or report. 
#' Can be used within the piping framework [see library(magrittr)].
#' 
#' @param .data the data.frame that is to be summarized
#' @param ... variables in the data set that are to be summarized; unquoted names separated by commas (e.g. age, gender, race) or indices. If indices, it needs to be a single vector (e.g. c(1:5, 8, 9:20) instead of 1:5, 8, 9:20). As it is currently, it CANNOT handle both indices and unquoted names simultaneously.
#' @param all logical; if set to \code{TRUE} all variables in the dataset are used. If there is a stratifying variable then that is the only variable excluded.
#' @param splitby the categorical variable to stratify by in formula form (e.g., \code{splitby = ~gender}) or quoted (e.g., \code{splitby = "gender"}); not too surprisingly, it requires that the number of levels be > 0
#' @param row_wise how to calculate percentages for factor variables when \code{splitby != NULL}: if \code{FALSE} calculates percentages by variable within groups; if \code{TRUE} calculates percentages across groups for one level of the factor variable.
#' @param splitby_labels allows for custom labels of the splitby levels; must match the number of levels of the splitby variable
#' @param medians a vector or list of continuous variables for which medians and 25\% and 75\% quartiles should be produced
#' @param test logical; if set to \code{TRUE} then the appropriate bivariate tests of significance are performed if splitby has more than 1 level
#' @param test_type has two options: "default" performs the default tests of significance only; "or" also give unadjusted odds ratios as well based on logistic regression (only use if splitby has 2 levels)
#' @param simple logical; if set to \code{TRUE} then only percentages are shown for categorical variables.
#' @param condense logical; if set to \code{TRUE} then continuous variables' means and SD's will be on the same line as the variable name and dichotomous variables only show counts and percentages for the reference category
#' @param piping if \code{TRUE} then the table is printed and the original data is passed on. It is very useful in piping situations where one wants the table but wants it to be part of a larger pipe.
#' @param rounding the number of digits after the decimal for means and SD's; default is 2
#' @param rounding_perc the number of digits after the decimal for percentages; default is 1
#' @param var_names custom variable names to be printed in the table
#' @param format_output has three options (with partial matching): 1) "full" provides the table with the type of test, test statistic, and the p-value for each variable; 2) "pvalues" provides the table with the p-values; and 3) "stars" provides the table with stars indicating significance. Only "p-values" works when \code{simple} and \code{condense} are set to TRUE
#' @param output_type default is "text"; the other options are all format options in the \code{kable()} function in \code{knitr} (e.g., latex, html, markdown, pandoc) as well as "text2" which adds a line below the header in the table.
#' @param format_number default in FALSE; if TRUE, then the numbers are formatted with commas (e.g., 20,000 instead of 20000)
#' @param NAkeep when sset to \code{TRUE} it also shows how many missing values are in the data for each categorical variable being summarized
#' @param m_label when \code{NAkeep = TRUE} this provides a label for the missing values in the table
#' @param booktabs when \code{output_type != "text"}; option is passed to \code{knitr::kable}
#' @param caption when \code{output_type != "text"}; option is passed to \code{knitr::kable}
#' @param align when \code{output_type != "text"}; option is passed to \code{knitr::kable}
#' @param export character; when given, it exports the table to a CSV file to folder named "table1" in the working directory with the name of the given string (e.g., "myfile" will save to "myfile.csv")
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
#'          splitby = ~a, 
#'          piping = TRUE) %>%
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
                  all = FALSE,
                  splitby = NULL, 
                  row_wise = FALSE, 
                  splitby_labels = NULL, 
                  medians = NULL,
                  test = FALSE, 
                  test_type = "default", 
                  simple = FALSE,
                  condense = FALSE,
                  piping = FALSE,
                  rounding = 2, 
                  rounding_perc = 1,
                  var_names = NULL, 
                  format_output = "pvalues", 
                  output_type = "text", 
                  format_number = FALSE,
                  NAkeep = FALSE, 
                  m_label = "Missing",
                  booktabs = TRUE, 
                  caption=NULL, 
                  align=NULL,
                  export=NULL){
  
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
  
  if (simple | condense){
    format_output = "pvalue"
  }
  
  ## All Variables or Selected Variables
  if (all){
    if (is.null(splitby)){
      data = .data
    } else {
      s_var = paste(splitby)[length(paste(splitby))]
      names(.data)
      data  = .data[!names(.data) %in% s_var]
    }
    
    ## Internal table1_ function
  } else {
    data = table1_(..., d_=.data, .cl=.call)
  }
  
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
  
  OR = NULL
  
  if (!is.null(splitby_labels))
    levels(d$split) = splitby_labels
  
  N = t(tapply(d[,1], d$split, length))
  
  ##############################
  # == # Summarizing Data # == #
  ##############################
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
      ## Means
      if (!nams[[i]] %in% medians){
        tab[[i]] = round(tapply(d[,i], d$split, mean, na.rm=TRUE), rounding)
        tab2[[i]] = round(tapply(d[,i], d$split, sd, na.rm=TRUE), rounding)
      } else if (nams[[i]] %in% medians){
        tab[[i]] = round(tapply(d[,i], d$split, median, na.rm=TRUE), rounding)
        tab2[[i]] = tapply(d[,i], d$split, function(x) paste0("[", suppressWarnings(formatC(round(IQR(x, na.rm=TRUE), rounding_perc), 
                                                                                            big.mark = f1, digits = 1, format = "f")), "]"))
      }
      if (any(N < 20)){
        warning("Tests are less robust to non-normality if N in any group is less than 20")
      }
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
      stop("Variables need to be either factor, character or numeric.")
    }
  }
  # === Finished Summarizing === #
  
  
  ##############################
  # == # Formatting Table # == # 
  ##############################
  ## == Not Condense == ##
  if (!condense){
    if (test){
      if (test_type=="or"){
        OR = data.frame(matrix(nrow=length(levels(d[,i]))+1, ncol=4))
        names(OR) = c(" ", "OR", "Lower", "Upper")
      }
      
      if (grepl("f|F", format_output))
        tabZ = data.frame(matrix(nrow=0, ncol=length(levels(d$split))+3))
      else if (grepl("p|P", format_output) | grepl("s|S", format_output))
        tabZ = data.frame(matrix(nrow=0, ncol=length(levels(d$split))+2))
    } else {
      tabZ = data.frame(matrix(nrow=0, ncol=length(levels(d$split))+1))
    }
    
    for (j in 1:length(tab)){
      if (is.factor(d[,j])){
        if (!grepl("^t", output_type)){
          tabX = data.frame(paste("--  ", names(table(d[,j], useNA=NAkeep)), "  --"))
        } else {
          tabX = data.frame(paste("  ", names(table(d[,j], useNA=NAkeep))))
        }
      } else if (is.numeric(d[,j])){
        tabX = data.frame(paste(" "))
      }
      
      ## Counts and Percentages or Just Percentages
      if (simple){
        for (i in 1:length(levels(d$split))){
          if (is.factor(d[,j])){
            tabX = data.frame(tabX, 
                              paste0(round(tab2[[j]][[i]]*100, rounding_perc), "%"))
          } else if (is.numeric(d[,j])){
            if (!nams[[j]] %in% medians){
              tabX = data.frame(tabX, 
                                paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1, digits = 2, format = "f")), " (", 
                                       suppressWarnings(formatC(tab2[[j]][[i]], big.mark = f1, digits = 2, format = "f")), ")"))
            } else if (nams[[j]] %in% medians){
              tabX = data.frame(tabX, 
                                paste(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1, digits = 2, format = "f")),
                                       tab2[[j]][[i]]))
            }        
          }
        }
      } else if (!simple){
        for (i in 1:length(levels(d$split))){
          if (is.factor(d[,j])){
            tabX = data.frame(tabX, 
                              paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1)), " (", 
                                     round(tab2[[j]][[i]]*100, rounding_perc), "%)"))
          } else if (is.numeric(d[,j])){
            if (!nams[[j]] %in% medians){
              tabX = data.frame(tabX, 
                                paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1, digits = 2, format = "f")), " (", 
                                       suppressWarnings(formatC(tab2[[j]][[i]], big.mark = f1, digits = 2, format = "f")), ")"))
            } else if (nams[[j]] %in% medians){
              tabX = data.frame(tabX, 
                                paste(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1, digits = 2, format = "f")),
                                       tab2[[j]][[i]]))
            }
          }
        }
      } else {
          stop("simple needs to be logical")
      }

      ## Optional Odds Ratio Table
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
  ## == Finished Not Condense == ##
    
    
  ## == Condense == ##
  } else if (condense){
    if (test){
      if (grepl("p|P", format_output) | grepl("s|S", format_output))
        tabZ = data.frame(matrix(nrow=0, ncol=length(levels(d$split))+2))
    } else {
      tabZ = data.frame(matrix(nrow=0, ncol=length(levels(d$split))+1))
    }
    
    for (j in 1:length(tab)){
      if (is.factor(d[,j])){
        if (length(levels(d[,j])) == 2){
          if (!grepl("text", output_type)){
            tabX = data.frame(paste0(names(d)[j], ": ", names(table(d[,j])[2])))
          } else {
            tabX = data.frame(paste0(names(d)[j], ": ", names(table(d[,j])[2])))
          }
        } else if (length(levels(d[,j])) > 2){
          if (!grepl("text", output_type)){
            tabX = data.frame(paste("--  ", names(table(d[,j]))))
          } else {
            tabX = data.frame(paste("  ", names(table(d[,j]))))
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
              tabX = data.frame(tabX, paste0(round(tab2[[j]][[i]]*100, 1), "%"))
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
          if (!nams[[j]] %in% medians){
            tabX = data.frame(tabX, 
                              paste0(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1, digits = 2, format = "f")), " (", 
                                     suppressWarnings(formatC(tab2[[j]][[i]], big.mark = f1, digits = 2, format = "f")), ")"))
          } else if (nams[[j]] %in% medians){
            tabX = data.frame(tabX, 
                              paste(suppressWarnings(formatC(tab[[j]][[i]], big.mark = f1, digits = 2, format = "f")),
                                    tab2[[j]][[i]]))
          }        
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
  }
  # === Finished Condense === #
  
  
  
  # === # Observations # === #
  N = suppressWarnings(formatC(N, big.mark = f1, digits = 0, format = "f"))
  
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
  if (!is.null(OR) & !condense){
    OR = rbind(tabQ, OR)
    final = cbind(final, OR)
    names(final)[4] = " "
  }
  final$` ` = as.character(final$` `)
  final$` `[is.na(final$` `)] = m_label
  
  ############################
  # === # FINAL OUTPUT # === #
  ############################
  if (length(levels(d$split)) == 1){
    names(final)[2] = "Mean/Count (SD/%)"
  }
  
  final_l = list(final)
  
  if (!is.null(export)){
    if (!dir.exists("Table1")){
      dir.create("Table1")
    }
    write.csv(final, file = paste0(getwd(), "/Table1/", export, ".csv"), row.names = FALSE)
  }
  
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


#' @export
print.table1 <- function(x, ...){
  ## Extract data set
  x2 = as.data.frame(x)
  x2[] = sapply(x2, as.character)
  ## Get width of table for lines
  max_col_width = list()
  for (i in 1:dim(x2)[2]){
    max_col_width[[i]] = max(sapply(x2[[i]], nchar, type="width"))
  }
  tot_width = sum(ifelse(unlist(max_col_width) > nchar(names(x2)), unlist(max_col_width), nchar(names(x2)))) + 
    dim(x2)[2] - 1
  ## Print
  cat("\n|")
  for (i in 1:tot_width){
    cat("=")
  }
  cat("|\n") 
  print(x[[1]], ..., row.names = FALSE, right = FALSE)
  cat("|")
  for (i in 1:tot_width){
    cat("=")
  }
  cat("|\n")
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
  df1 = NULL
  vars = eval(substitute(alist(...)))
  
  if (length(vars) == 1){
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




#' Table 1 for Missing Data Analysis
#' 
#' Produces a descriptive table, stratified a variable with missing values, 
#' providing means/frequencies and standard deviations/percentages. 
#' It is well-formatted for easy transition to academic article or report. 
#' Can be used within the piping framework [see library(magrittr)].
#' 
#' @param .data the data.frame that is to be summarized
#' @param ... variables in the data set that are to be summarized; unquoted names separated by commas (e.g. age, gender, race) or indices. If indices, it needs to be a single vector (e.g. c(1:5, 8, 9:20) instead of 1:5, 8, 9:20). As it is currently, it CANNOT handle both indices and unquoted names simultaneously.
#' @param all logical; if set to \code{TRUE} all variables in the dataset are used. If there is a stratifying variable then that is the only variable excluded.
#' @param missing_var the categorical variable to stratify by in formula form (e.g., \code{splitby = ~gender}) or quoted (e.g., \code{splitby = "gender"}); not too surprisingly, it requires that the number of levels be > 0
#' @param row_wise how to calculate percentages for factor variables when \code{splitby != NULL}: if \code{FALSE} calculates percentages by variable within groups; if \code{TRUE} calculates percentages across groups for one level of the factor variable.
#' @param splitby_labels allows for custom labels of the splitby levels; must match the number of levels of the splitby variable
#' @param medians a vector or list of continuous variables for which medians and 25\% and 75\% quartiles should be produced
#' @param test logical; if set to \code{TRUE} then the appropriate bivariate tests of significance are performed if splitby has more than 1 level
#' @param test_type has two options: "default" performs the default tests of significance only; "or" also give unadjusted odds ratios as well based on logistic regression (only use if splitby has 2 levels)
#' @param simple logical; if set to \code{TRUE} then only percentages are shown for categorical variables.
#' @param condense logical; if set to \code{TRUE} then continuous variables' means and SD's will be on the same line as the variable name and dichotomous variables only show counts and percentages for the reference category
#' @param piping if \code{TRUE} then the table is printed and the original data is passed on. It is very useful in piping situations where one wants the table but wants it to be part of a larger pipe.
#' @param rounding the number of digits after the decimal for means and SD's; default is 2
#' @param var_names custom variable names to be printed in the table
#' @param format_output has three options (with partial matching): 1) "full" provides the table with the type of test, test statistic, and the p-value for each variable; 2) "pvalues" provides the table with the p-values; and 3) "stars" provides the table with stars indicating significance
#' @param output_type default is "text"; the other options are all format options in the \code{kable()} function in \code{knitr} (e.g., latex, html, markdown, pandoc) as well as "text2" which adds a line below the header in the table.
#' @param format_number default in FALSE; if TRUE, then the numbers are formatted with commas (e.g., 20,000 instead of 20000)
#' @param NAkeep when sset to \code{TRUE} it also shows how many missing values are in the data for each categorical variable being summarized
#' @param m_label when \code{NAkeep = TRUE} this provides a label for the missing values in the table
#' @param booktabs when \code{output_type != "text"}; option is passed to \code{knitr::kable}
#' @param caption when \code{output_type != "text"}; option is passed to \code{knitr::kable}
#' @param align when \code{output_type != "text"}; option is passed to \code{knitr::kable}
#' @param export character; when given, it exports the table to a CSV file to folder named "table1" in the working directory with the name of the given string (e.g., "myfile" will save to "myfile.csv")
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
#'          splitby = ~a, 
#'          piping = TRUE) %>%
#'   summarise(count = n())
#' 
#' ## Adjust variables within function
#' table1(df, ifelse(x > 0, 1, 0), z,
#'        var_names = c("X2", "Z"))
#'          
#'
#' @export
#' @import stats
#' @importFrom knitr kable
tableM = function(.data, 
                  ..., 
                  all = FALSE,
                  missing_var = NULL, 
                  row_wise = FALSE, 
                  splitby_labels = NULL, 
                  medians = NULL,
                  test = FALSE, 
                  test_type = "default", 
                  simple = FALSE,
                  condense = FALSE,
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
                  align=NULL,
                  export=NULL){
  
  .d = .data
  .d$splitby2 = eval(parse(text = paste(missing_var)[[length(paste(missing_var))]]), .data)
  
  tabM = table1(.data = .d, 
         ..., 
         all = all,
         splitby = ~factor(is.na(splitby2)), 
         row_wise = row_wise, 
         splitby_labels = splitby_labels, 
         medians = medians,
         test = test, 
         test_type = test_type, 
         simple = simple,
         condense = condense,
         rounding = rounding, 
         var_names = var_names, 
         format_output = format_output, 
         output_type = output_type, 
         format_number = format_number,
         NAkeep = NAkeep, 
         m_label = m_label,
         booktabs = booktabs, 
         caption=caption, 
         align=align,
         export=export)
  
  if (piping){
    print(tabM)
    invisible(.data)
  } else {
    return(tabM)
  }
}


