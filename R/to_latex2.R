#' From Table 1 to Latex 2
#' 
#' Internal \code{table1()} and \code{tableC()} function for providing output = "latex2"
#' 
#' @param tab the table1 object
#' @param caption caption character vector
#' @param align align character vector
#' @param len the number of levels of the grouping factor
#' @param splitby the name of the grouping factor
#' @param float argument for latex formatting
#' @param cor_type optional argument regarding the correlation type (for tableC)
#' @param booktabs add booktabs to latex table
#' @param label latex label option
#' @param total is there a total column (from Table 1) to be printed?
#' 
#' @export
#' @importFrom utils capture.output
to_latex = function(tab, caption, align, len, splitby, float, booktabs, label, total=FALSE, cor_type=NULL){
  if (is.null(cor_type) && is.null(splitby)){
    splitby <- "Total"
  } else if (!is.null(cor_type)){
    cor_type2 <- paste(toupper(substr(cor_type, 1, 1)), substring(cor_type, 2),
                       sep = "")
    splitby <- paste(cor_type2, "Correlations")
  } else if (is.null(cor_type) && !is.null(splitby)) {
    splitby <- gsub("`", "", paste(splitby))
    splitby <- gsub("%", "\\%", splitby)
  }
  if (total) tot_column <- " & " else tot_column <- ""
  
  ## Fix problematic latex characters
  tab[] <- lapply(tab, function(x) gsub("%", "\\%", x, fixed = TRUE))
  tab[] <- lapply(tab, function(x) gsub("NA", "\\emph{missing}", x, fixed = TRUE))
  tab[] <- lapply(tab, function(x) gsub("_", "\\_", x, fixed = TRUE))
  
  ## Produce latex table
  out <- capture.output({
    cat("\\begin{table}[", float, "] \n")
    cat("\\centering \n")
    cat("\\caption{", caption, "}", "\\label{", ifelse(is.null(label), "", label), "}\n", sep = "")
    cat("\\begin{tabular}{", align, "}\n")
    cat(hrule('top', booktabs))
    cat(" & ", tot_column, "\\multicolumn{", paste0(len), "}{c}{", ifelse(is.null(splitby), "Total", splitby), "}\\\\ \n")
    
    if (is.null(cor_type)){
      cat(paste(gsub("%", "\\%", names(tab), fixed = TRUE), collapse = " & "), "\\\\", "\n")
      cat(paste(tab[1, ], collapse = " & "), "\\\\ \n", hrule('mid', booktabs))
      
      cat(
        for (i in 2:length(tab[[1]])){
          if (grepl("^ ", tab[i, 1])){
            cat("\\hspace{6pt}", paste(tab[i, ], collapse = " & "))
            cat("\\\\", "\n")
          } else {
            cat(paste(tab[i, ], collapse = " & "))
            cat("\\\\", "\n")
          }
          
        })
    } else {
      cat(paste(names(tab), collapse = " & "), "\\\\", paste0("\n",hrule('mid', booktabs)))
      cat(
        for (i in seq_along(tab[[1]])){
          if (grepl("^ ", tab[i, 1])){
            cat("\\hspace{6pt}", paste(tab[i, ], collapse = " & "))
            cat("\\\\", "\n")
          } else {
            cat(paste(tab[i, ], collapse = " & "))
            cat("\\\\", "\n")
          }
        }
      )
    }
    cat(paste0(c(hrule('bottom', booktabs),
      "\\end{tabular}", 
      "\\end{table}\n"), collapse="\n"))
  })
  class(out) <- c("latex2", "character", "table1")
  out
}

#' @export
print.latex2 = function(x, ...){
  cat(paste(x, collapse = "\n"))
}

                 
hrule <- function(location, booktabs) {
  if (booktabs) {
    if (location == 'top') {
      "\\toprule\n"
    } else if (location == 'mid') {
      "\\midrule\n"
    } else if (location == 'bottom') {
      "\\bottomrule\n"
    } else { stop(location) }
  } else {
    "\\hline\n"
  }
}
