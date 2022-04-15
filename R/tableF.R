#' Frequency Table
#'
#' Provides in-depth frequency counts and percentages.
#' 
#' @param .data the data frame containing the variable
#' @param x the bare variable name (not quoted)
#' @param n the number of values shown int he table
#' @param splitby the stratifying variable
#'
#' @return a list of class \code{tableF} containing the frequency table(s)
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(furniture)
#' 
#' data <- data.frame(
#'   x = sample(c(1,2,3,4), 100, replace=TRUE),
#'   y = rnorm(100)
#' )
#' 
#' ## Basic Use
#' tableF(data, x)
#' tableF(data, y)
#' 
#' ## Adjust the number of items shown
#' tableF(data, y, n = 10)
#' 
#' ## Add splitby
#' tableF(data, x, splitby = y)
#' 
#' }
#'
#' @export

tableF <- function(.data, x, n = 20, splitby = NULL){
  .call <- match.call()
  x <- eval(substitute(x), .data)
  
  if (is.null(attr(.data, "vars")) && is.null(attr(.data, "groups"))){
    
    ### Splitby Variable (adds the variable to d as "split")
    splitby = substitute(splitby)
    if (inherits(substitute(splitby), "name")){
      splitby_ = eval(substitute(splitby), .data)
    } else if (inherits(substitute(splitby), "call")){
      splitby_ = model.frame(splitby, .data, na.action = "na.pass")[[1]]
    } else if (inherits(substitute(splitby), "character")){
      splitby_ = .data[[splitby]]
    } else if(is.null(splitby)){
      splitby_ = factor(1, labels = paste(.call[3]))
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
      splitby_ <- factor(.data[[groups]])
    } else {
      interacts <- interaction(.data[groups], sep = "_")
      splitby_ <- factor(interacts)
    }
  }
  
  if(any(is.na(splitby_))){
    splitby_ <- factor(ifelse(is.na(splitby_),"Missing", splitby_))
  }
  
  ## Error catch for all missing
  if(all(is.na(x))){
    warn <- paste0("All values for ",  .call[3], " are missing.")
    warning(warn)
    return(NULL)
  }
  
  splitby_ <- factor(splitby_)
  
  final_list <- list()
  for(i in levels(splitby_)){
    
    x1 <- x[splitby_ == i & !is.na(splitby_)]
    
    ## Summary statistics
    Freq     <- table(x1, useNA="ifany")
    CumFreq  <- round(cumsum(table(x1, useNA="ifany")))
    Percent  <- suppressWarnings(formatC(100*(prop.table(table(x1, useNA="ifany"))), 
                                         format = "f", digits = 2, big.mark = ","))
    CumPerc  <- suppressWarnings(formatC(100*cumsum(prop.table(table(x1, useNA="ifany"))), 
                                         format = "f", digits = 2, big.mark = ","))
    Valid    <- suppressWarnings(formatC(100*(prop.table(table(x1, useNA="no"))), 
                                         format = "f", digits = 2, big.mark = ","))
    CumValid <- suppressWarnings(formatC(100*cumsum(prop.table(table(x1, useNA="no"))), 
                                         format = "f", digits = 2, big.mark = ","))
    
    ## If there is missing, add a blank line below the valids
    if (any(is.na(x1))){
      names(Freq)[length(names(Freq))] <- "Missing"
      names(CumFreq)[length(names(CumFreq))] <- "Missing"
      final <- data.frame("Var"     = names(Freq),
                         "Freq"    = as.character(Freq), 
                         "CumFreq" = as.character(CumFreq),  
                         "Percent" = paste0(Percent, "%"), 
                         "CumPerc" = paste0(CumPerc, "%"),
                         "Valid"   = c(paste0(Valid, "%"), ""),
                         "CumValid" = c(paste0(CumValid, "%"), ""), 
                         stringsAsFactors = TRUE)
      names(final)[1] <- paste(i)
      final[] <- lapply(final,as.character)
      
      if (dim(final)[1] > n){
        final1 <- final[c(1:(n/2)),]
        final2 <- final[c((dim(final)[1] - n/2):(dim(final)[1])),]
        final1 <- rbind(final1, "...")
        final  <- rbind(final1, final2)
        row.names(final) =  ifelse(final$Freq=="...", "...", row.names(final))
      }
      
    } else {
      final <- data.frame("Var"     = names(Freq),
                          "Freq"    = as.character(Freq), 
                          "CumFreq" = as.character(CumFreq), 
                          "Percent" = paste0(Percent, "%"), 
                          "CumPerc" = paste0(CumPerc, "%"), 
                          stringsAsFactors = TRUE)
      names(final)[1] <- paste(i)
      final[] <- lapply(final,as.character)
      
      if (dim(final)[1] > n){
        final1 <- final[c(1:(n/2)),]
        final2 <- final[c((dim(final)[1] - n/2):(dim(final)[1])),]
        final1 <- rbind(final1, "...")
        final  <- rbind(final1, final2)
        row.names(final) <- ifelse(final$Freq=="...", "...", row.names(final))
      }}
    
    final_list[[i]] <- final  
  }
  
  ## Output
  class(final_list) <- c("tableF", "list")
  attr(final_list, "variable") <- paste(.call[3])
  final_list
}

#' @export
print.tableF <- function(x, ...){
  max_col_width = max_col_width2 = list()
  len = length(x)
  
  if (len > 1){
    message("Variable:", attr(x, "variable"))
  }
  
  for (i in 1:len){
    x2 = as.data.frame(x[[i]], stringsAsFactors = TRUE)
    x2[] = sapply(x2, as.character)
    
    ## Get width of table for lines
    for (j in 1:dim(x2)[2]){
      max_col_width[[j]] = max(sapply(x2[[j]], nchar, type="width"))
    }
    tot_width = sum(ifelse(unlist(max_col_width) > nchar(names(x2)), unlist(max_col_width), nchar(names(x2)))) + 
      dim(x2)[2] - 1
    
    ## Print top border
    cat("\n\u2500")
    for (j in 1:tot_width){
      cat("\u2500")
    }
    cat("\u2500\n") 
    ## Print table
    print(x2, ..., row.names = FALSE, right = FALSE)
    ## Print bottom border
    cat("\u2500")
    for (j in 1:tot_width){
      cat("\u2500")
    }
    cat("\u2500\n")
  }
}

