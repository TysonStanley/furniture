#' @title flextable output for table1
#'
#' @description This takes a table1 object and outputs a `flextable` version.
#'
#' @param tab the table1 object
#' @param spanner the label above the grouping variable (if table1 is grouped)
#' or any label you want to include over the statistics column(s)
#'
#' @importFrom flextable flextable
#' @importFrom flextable add_header_row
#' @importFrom flextable align
#' @importFrom flextable padding
#' @importFrom flextable autofit
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
#'   table1_flextable(spanner = "Asthma")
#'
#' @export
table1_flextable <- function(tab, spanner = NULL) {
  nams <- names(tab[[1]])
  nams[1] <- "Characteristic"
  tab_df <- as.data.frame(tab)
  nams <- paste0(nams, tab_df[1, ])
  nams <- gsub("n =", ", n =", nams)
  nams <- gsub("[[:space:]]*$","", nams)
  tab_df <- tab_df[-1, ]
  names(tab_df) <- nams

  # detect which rows are indented (have leading spaces)
  indented_rows <- grepl("^[[:space:]]{2}", tab_df$Characteristic)

  # create flextable
  ft <- flextable::flextable(tab_df)

  # add padding for indented rows
  if (any(indented_rows)) {
    ft <- flextable::padding(ft,
                             i = which(indented_rows),
                             j = 1,
                             padding.left = 20)
  }

  # align columns - left for characteristic, center/right for statistics
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  if (ncol(tab_df) > 1) {
    ft <- flextable::align(ft, j = 2:ncol(tab_df), align = "center", part = "all")
  }

  # add spanner header row if provided
  if (!is.null(spanner) && ncol(tab_df) > 1) {
    ft <- flextable::add_header_row(ft,
                                    values = c("", spanner),
                                    colwidths = c(1, ncol(tab_df) - 1))
  }

  # autofit for better appearance
  ft <- flextable::autofit(ft)

  return(ft)
}
