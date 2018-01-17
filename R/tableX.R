#' Table X (for Cross-Tabs)
#' 
#' Provides a pipe-able, clean, flexible version of \code{table()}.
#' 
#' @param .data the data frame containing the variables
#' @param x1 the first bare (not quoted) variable found in .data
#' @param x2 the second bare (not quoted) variable found in .data
#' @param type the summarized output type; can be "count", "cell_perc", "row_perc", or "col_perc"
#' @param na.rm logical; whether missing values should be removed
#' @param format_number default is FALSE; if TRUE, then the numbers are formatted with commas (e.g., 20,000 instead of 20000)
#' 
#' @import stats
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(furniture)
#' library(tidyverse)
#' 
#' data <- data.frame(
#'   x = sample(c(1,2,3,4), 100, replace=TRUE),
#'   y = sample(c(0,1), 100, replace=TRUE)
#' )
#' 
#' tableX(data, x, y)
#' 
#' data %>%
#'   tableX(x, y)
#' 
#' data %>%
#'   tableX(x, y, na.rm = TRUE)
#' 
#' }
#' 
#' @export
tableX = function(.data, x1, x2, type = "count", na.rm = FALSE, format_number = FALSE){
  
  .call = match.call()
  x1 = eval(substitute(x1), .data)
  x2 = eval(substitute(x2), .data)
  
  ## Missing Data
  if(grepl("F|f", na.rm)){
    if(any(is.na(x1))){
      x1 = factor(ifelse(is.na(x1),"Missing", x1))
    }
    
    if(any(is.na(x2))){
      x2 = factor(ifelse(is.na(x2),"Missing", x2))
    }
  }
  
  
  ## Changing "Sum" to "Total" and "Sum" to "All"
  Total <- sum
  All <- sum
  
  ## Format Number
  big.mark = ""
  if (format_number) big.mark = ","
  
  ## type: Counts
  if(type %in% c("count")){
    final = noquote(
      suppressWarnings(
        formatC(
          addmargins(
            table(x1, x2, 
                  useNA = "no",
                  dnn = c(as.character(.call[3]), as.character(.call[4]))), 
            FUN = Total, 
            quiet = TRUE),
          format = "f", 
          digits = 0, 
          big.mark = big.mark)
      )
    )
    
  } 
  
  ## type: Cell Percentage
  else if(type %in% c("cell_perc")){
    final = noquote(
      suppressWarnings(
        formatC(
          addmargins(
            100*(prop.table(
              table(
                x1, x2, useNA = "no", 
                dnn =  c(as.character(.call[3]), as.character(.call[4]))))), 
            FUN = Total, quiet = TRUE), 
          format = "f", digits = 2, 
          big.mark = big.mark)))
    
  } 
  
  ## type: Row Percentage
  else if(type %in% c("row_perc")){
    final = noquote(
      suppressWarnings(
        formatC(
          addmargins(
            100*prop.table(
              addmargins(
                table(x1, x2, 
                      useNA = "no",
                      dnn = c(as.character(.call[3]), as.character(.call[4]))),
                margin = 1, 
                FUN = All, 
                quiet = TRUE),
              margin = 1),
            margin = 2, 
            FUN = Total, 
            quiet = TRUE), 
          format = "f", 
          digits = 2, 
          big.mark = big.mark)
      )
    )
  } 
  
  ## type: Column Percentage
  else if(type %in% c("col_perc")){
    final = noquote(
      suppressWarnings(
        formatC(
          addmargins(
            100*prop.table(
              addmargins(
                table(x1, x2, 
                      useNA = "no",
                      dnn = c(as.character(.call[3]), as.character(.call[4]))), 
                margin = 2, 
                FUN = All, 
                quiet = TRUE),
              margin = 2),
            margin = 1, 
            FUN = Total, 
            quiet = TRUE),
          format = "f", 
          digits = 2, 
          big.mark = big.mark)
      )
    )
  }
  class(final) = 'table'
  final
}

