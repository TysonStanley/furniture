#' @title gt output for table1
#'
#' @description This takes a table1 object and outputs a `gt` version.
#'
#' @param tab the table1 object
#' @param spanner the label above the grouping variable (if table1 is grouped)
#' or any label you want to include over the statistics column(s)
#'
#' @importFrom gt gt
#' @importFrom gt fmt_markdown
#' @importFrom gt tab_spanner
#'
#' @examples
#'
#' library(furniture)
#' library(dplyr)
#'
#' data('nhanes_2010')
#' nhanes_2010 %>%
#'   group_by(asthma) %>%
#'   table1(age, marijuana, illicit, rehab, na.rm = FALSE) %>%
#'   table1_gt(spanner = "Asthma")
#'
#' @export
table1_gt <- function(tab, spanner = NULL) {
  nams <- names(tab[[1]])
  nams[1] <- "Characteristic"
  tab_df <- as.data.frame(tab)
  nams <- paste0(nams, tab_df[1, ])
  nams <- gsub("n =", ", n =", nams)
  nams <- gsub("[[:space:]]*$","", nams)
  tab_df <- tab_df[-1, ]
  names(tab_df) <- nams

  # add spacing for the table
  tab_df$Characteristic <- ifelse(
    grepl("  ", tab_df$Characteristic),
    paste("&nbsp;&nbsp;&nbsp;&nbsp;", tab_df$Characteristic),
    tab_df$Characteristic
  )

  # make it a gt and return
  gt_tab <- gt::gt(tab_df)
  gt_tab <- gt::fmt_markdown(gt_tab)

  # add spanner
  if (!is.null(spanner))
    gt::tab_spanner(gt_tab, label = spanner, columns = -Characteristic)
  else
    gt_tab
}
