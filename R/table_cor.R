#' Correlation Table
#' 
#' Correlations printed in a nicely formatted table.
#' 
#' @param .data the data frame containing the variables
#' @param ... the unquoted variable names to be included in the correlations
#' @param cor_type the correlation type; default is "pearson", other option is "spearman"
#' @param na.rm logical (default is \code{FALSE}); if set to \code{TRUE}, the correlations use the "complete.obs" methods option from \code{stats::cor()}
#' @param rounding the value passed to \code{round} for the output of both the correlation and p-value; default is 3
#' @param output how the table is output; can be "text" for regular console output, "latex2" for specialized latex output, or any of \code{kable()}'s options from \code{knitr} (e.g., "latex", "markdown", "pandoc").
#' @param booktabs when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param caption when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param align when \code{output != "text"}; option is passed to \code{knitr::kable}
#' @param float when \code{output == "latex2"} it controls the floating parameter (h, t, b, H)
#'
#' @seealso stats::cor
#' 
#' @importFrom stats cor
#' @importFrom knitr kable
#' 
#' @export
tableC <- function(.data, 
                   ..., 
                   cor_type = "pearson",
                   na.rm = FALSE,
                   rounding = 3,
                   output = "text",
                   booktabs = TRUE, 
                   caption = NULL, 
                   align = NULL,
                   float = "htb"){
  
  ## Preprocessing ##
  .call <- match.call()
  data <- selecting(d_=.data, ...)
  d <- as.data.frame(data, stringsAsFactors = TRUE)
  
  ## NA ##
  if (na.rm){
    use1 <- "complete.obs"
    n <- sum(complete.cases(d))
  } else {
    use1 <- "everything"
    n <- length(d[[1]])
  }
  
  ## Correlations ##
  cors <- stats::cor(d, 
                     method = cor_type,
                     use = use1)
  ## Significance ##
  if (cor_type == "pearson"){
    tvalues <- cors/sqrt((1 - cors^2)/(n-2))
  } else if (cor_type == "spearman"){
    tvalues <- cors * sqrt((n-2)/(1 - cors^2))
  } else {
    stop(paste(cor_type, "is not a possible correlation type with this function."))
  }

  pvalues <- 2*pt(abs(tvalues), n-2, lower.tail = FALSE)
  
  ## Formatting Names and Rownames
  cors <- as.data.frame(cors, stringsAsFactors = TRUE)
  pvalues <- as.data.frame(pvalues, stringsAsFactors = TRUE)
  dims <- dim(cors)
  
  if (output == "latex2"){
    row.names(cors) <- paste0("{[", 1:dims[1], "]}", row.names(cors))
  } else {
    row.names(cors) <- paste0("[", 1:dims[1], "]", row.names(cors))
  }
  
  names(cors) <- paste0("[", 1:dims[1], "]")
  
  ## Combine
  final <- matrix(nrow = dims[1], ncol = dims[2])
  for (i in 1:dims[1]){
    for (j in 1:dims[2]){
      final[i,j] <- paste0(ifelse(cors[i,j] == 1, "1.00", round(cors[i,j], rounding)), 
                           " ", 
                           ifelse(cors[i,j] == 1, "",
                           ifelse(pvalues[i,j] < .001, "(<.001)", 
                                  paste0("(", 
                                         round(pvalues[i,j], rounding), ")"))))
    }
  }
  final[upper.tri(final)] <- " "
  final <- as.data.frame(final, stringsAsFactors = TRUE)
  row.names(final) <- row.names(cors)
  final <- data.frame(" " = row.names(final), final, stringsAsFactors = TRUE)
  names(final) <- c(" ", names(cors))
  
  ## Output ##
  message("N = ", n, "\n",
          "Note: ", cor_type, " correlation (p-value).")
  
  if (output != "text"){
    if (output == "latex2"){
      if (is.null(align)){
        l1 <- dim(final)[2]
        align <- c("l", rep("c", (l1-1)))
      }
      tab <- to_latex(final, caption, align, len = dim(final)[2] - 1, splitby = NA, float, booktabs, cor_type)
      return(tab)
      
    } else {
      kab <- knitr::kable(final, 
                          format=output,
                          booktabs = booktabs,
                          caption = caption,
                          align = align,
                          row.names = FALSE)
      return(kab)
    }
  } else {
    
    final <- list("Table1" = final)
    class(final) <- c("table1", "list")
    attr(final, "splitby") <- NULL
    attr(final, "output") <- NULL
    return(final)
  }
}
