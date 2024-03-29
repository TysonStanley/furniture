% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table1_gt.R
\name{table1_gt}
\alias{table1_gt}
\title{gt output for table1}
\usage{
table1_gt(tab, spanner = NULL)
}
\arguments{
\item{tab}{the table1 object}

\item{spanner}{the label above the grouping variable (if table1 is grouped)
or any label you want to include over the statistics column(s)}
}
\description{
This takes a table1 object and outputs a `gt` version.
}
\examples{

library(furniture)
library(dplyr)

data('nhanes_2010')
nhanes_2010 \%>\%
  group_by(asthma) \%>\%
  table1(age, marijuana, illicit, rehab, na.rm = FALSE) \%>\%
  table1_gt(spanner = "Asthma")

}
\author{
Tyson S. Barrett
}
