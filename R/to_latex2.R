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
#' 
#' @export
#' @import utils
#' 
to_latex = function(tab, caption, align, len, splitby, float, cor_type=NULL){
  if (is.null(cor_type) & is.null(splitby)){
    splitby = "Total"
  } else if (!is.null(cor_type)){
    cor_type2 = paste(toupper(substr(cor_type, 1, 1)), substring(cor_type, 2),
                      sep = "")
    splitby = paste(cor_type2, "Correlations")
  } else if (is.null(cor_type) & !is.null(splitby)) {
    splitby = gsub("`", "", paste(splitby))
  }
  
  tab[] = lapply(tab, function(x) gsub("%", "\\%", x, fixed = TRUE))
  
  out = capture.output({
    cat("\\begin{table}[", float, "] \n")
    cat("\\centering \n")
    cat("\\caption{", caption, "}\n", sep = "")
    cat("\\begin{tabular}{", align, "}\n")
    cat("\\hline \n")
    cat(" & \\multicolumn{", paste0(len), "}{c}{", paste(splitby)[length(paste(splitby))], "}\\\\ \n")
    
    if (is.null(cor_type)){
      cat(paste(names(tab), collapse = " & "), "\\\\", "\n")
      cat(paste(tab[1, ], collapse = " & "), "\\\\ \n", "\\hline \n")
      
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
      cat(paste(names(tab), collapse = " & "), "\\\\", "\n \\hline \n")
      cat(
        for (i in 1:length(tab[[1]])){
          if (grepl("^ ", tab[i, 1])){
            cat("\\hspace{6pt}", paste(tab[i, ], collapse = " & "))
            cat("\\\\", "\n")
          } else {
            cat(paste(tab[i, ], collapse = " & "))
            cat("\\\\", "\n")
          }
        })
    }
    cat(paste0(c("\\hline",
      "\\end{tabular}",
      "\\end{table}\n"), collapse="\n"))
  })
  class(out) = c("latex2", "character", "table1")
  out
}

#' @export
print.latex2 = function(x, ...){
  cat(paste(x, collapse = "\n"))
}