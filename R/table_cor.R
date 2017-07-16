#' Correlation Table
#' 
#' Correlations printed in a nicely formatted table.
#' 
#' @param .data the data frame containing the variables
#' @param ... the unquoted variable names to be included in the correlations
#' @param cor_type the correlation type; default is "pearson", other options are "spearman" and "kendall"
#' @param na.rm logical (default is \code{FALSE}); if set to \code{TRUE}, the correlations use the "complete.obs" methods option from \code{stats::cor()}
#' @param rounding the value passed to \code{round} for the output of both the correlation and p-value; default is 3
#' @param output how the table is output; can be "text" for regular console output or any of \code{kable()}'s options from \code{knitr} (e.g., "latex", "markdown", "pandoc").
#' @param booktabs when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param caption when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param align when \code{output != "text"}; option is passed to \code{knitr::kable}
#'
#' @seealso stats::cor
#' 
#' @importFrom stats cor
#' @importFrom knitr kable
#' 
#' @export
tableC = function(.data, 
                  ..., 
                  cor_type = "pearson",
                  na.rm = FALSE,
                  rounding = 3,
                  output = "text",
                  booktabs = TRUE, 
                  caption = NULL, 
                  align = NULL){
  
  ## Preprocessing ##
  .call = match.call()
  data = table1_(..., d_=.data, .cl=.call)
  d = as.data.frame(data)
  
  ## NA ##
  if (na.rm){
    use1 = "complete.obs"
    n = sum(with(d, complete.cases(...)))
  } else {
    use1 = "everything"
    n = length(d[[1]])
  }
  
  ## Correlations ##
  cors = cor(d, 
              method = cor_type,
              use = use1)
  ## Significance ##
  tvalues = cors/sqrt((1 - cors^2)/(n-2))
  pvalues = 2*pt(abs(tvalues), n-2, lower.tail = FALSE)
  
  ## Formatting Names and Rownames
  cors = as.data.frame(cors)
  pvalues = as.data.frame(pvalues)
  dims = dim(cors)
  row.names(cors) = paste0("[", 1:dims[1], "]", row.names(cors))
  names(cors) = paste0("[", 1:dims[1], "]")
  
  ## Combine
  final = matrix(nrow = dims[1], ncol = dims[2])
  for (i in 1:dims[1]){
    for (j in 1:dims[2]){
      final[i,j] = paste0(ifelse(cors[i,j] == 1, "1.00", round(cors[i,j], rounding)), 
                          " ", 
                          ifelse(cors[i,j] == 1, "",
                          ifelse(pvalues[i,j] < .001, "(<.001)", 
                                 paste0("(", 
                                        round(pvalues[i,j], rounding), ")"))))
    }
  }
  final[upper.tri(final)] <- " "
  final = as.data.frame(final)
  row.names(final) = row.names(cors)
  final = data.frame(" " = row.names(final), final)
  names(final) = c(" ", names(cors))
  
  ## Output ##
  message("N = ", n, "\n",
          "P-values are between the parantheses.")
  if (output != "text"){
    kab = knitr::kable(final, format=output,
                       booktabs = booktabs,
                       caption = caption,
                       align = align,
                       row.names = FALSE)
    return(kab)
  } else {
    final = list("Table1" = final,
                 "NULL" = NULL)
    class(final) = c("table1", "list")
    return(final)
  }
}