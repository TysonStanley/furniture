\name{table1}
\alias{table1}
\title{Table 1 for Health, Behavioral, and Social Scientists}
\description{ Produces a descriptive table, stratified by an optional categorical variable, providing means/frequencies and standard deviations/percentages. It is well-formatted for easy transition to academic article or report. Can be used within the piping framework [see \code{library(magrittr)}].}

\usage{
table1(.data, ..., splitby=NULL, splitby_labels=NULL, 
       test=FALSE, test.type="default", 
       piping = FALSE, rounding=3, var.names=NULL, 
       format.output="full", output.type="text", 
       NAkeep = FALSE, m_label = "Missing",
       booktabs = TRUE, caption=NULL, align=NULL)
}

\arguments{
  \item{.data}{
      the data.frame that is to be summarized
}
  \item{\dots}{
      variables in the data set that are to be summarized; unquoted names separated by commas (e.g. \code{age, gender, race}) or indices. If indices, it needs to be a single vector (e.g. \code{c(1:5, 8, 9:20)} instead of \code{1:5, 8, 9:20}). As it is currently, it CANNOT handle both indices and unquoted names simultaneously.
}
  \item{splitby}{
      the categorical variable to stratify by in formula form (e.g., \code{~gender}); requires \code{levels(splitby)>0}
}
  \item{splitby_labels}{
      allows for custom labels of the splitby levels; must match the number of levels of the splitby variable
}
  \item{test}{
      logical; if set to \code{TRUE} then bivariate tests of significance are performed; requires \code{levels(splitby)>1}; automatically performs the correct test based on variable types and number of levels
}
  \item{test.type}{
       has two options: "default" performs the default tests of significance only; "or" gives unadjusted odds ratios as well based on logistic regression and currently should only be used when the splitting variable has only 2 levels.
}
  \item{piping}{
       if \code{TRUE} then the table is printed and the original data is passed on. It is very useful in piping situations where one wants the table but wants it to be part of a larger pipe. if \code{FALSE} it returns the table1 object only.
}
  \item{rounding}{
      the number of digits after the decimal; default is 3
}
  \item{var.names}{
      custom variable names to be printed in the table
}
  \item{format.output}{
      has three options: 1) "full" provides the table with the type of test, test statistic, and the p-value for each variable; 2) "pvalues" provides the table with the p-values; 3) "stars" provides the table with stars indicating significance
}
  \item{output.type}{
      default is "text"; the other option is "latex" which uses the \code{kable} function in \code{knitr}
}
  \item{NAkeep}{
      when set to \code{TRUE} it also shows how many missing values are in the data by variable
}
  \item{m_label}{
      when \code{NAkeep = TRUE} this provides a label for the missing values in the table
}
  \item{booktabs}{
      when \code{output.type = "latex"}; option is passed to \code{knitr::kable}; default is \code{TRUE}
}
  \item{caption}{
      when \code{output.type = "latex"} option is passed to \code{knitr::kable}; default is \code{NULL}
}
  \item{align}{
      when \code{output.type = "latex"} option is passed to \code{knitr::kable}; default is \code{NULL}
}
}

\value{
A table with the number of observations, means/frequencies and standard deviations/percentages.The object is a \code{table1} class object with a print method that prints without row numbering and a few other minor adjustments. Can be printed in LaTex form.
}

\author{Tyson Barrett and Emily Brignone}

\seealso{
\code{\link{print.table1}}
}

\examples{
## Data from MASS package ##
library(MASS)
data("birthwt")

## Using dplyr
library(dplyr)
b = mutate(.data=birthwt,
           smoke = as.factor(smoke),
           race  = as.factor(race),
           ht    = as.factor(ht),
           ui    = as.factor(ui))
levels(b$race) = c("white", "black", "other")

## Table1 examples
table1(b, age, race, smoke, ptl, ht, ui, ftv,
       NAkeep=TRUE)

table1(b, age, race, smoke, ptl, ht, ui, ftv,
       splitby=~low,
       NAkeep=TRUE)

table1(b, age, race, smoke, ptl, ht, ui, ftv,
       splitby=~low,
       test=TRUE)

table1(b, age, race, smoke, ptl, ht, ui, ftv,
       splitby=~low,
       test=TRUE,
       var.names = c("Age", "Race", "Smoking Status", "Previous Premature Labors", "Hypertension",
                     "Uterine Irratibility", "Physician Visits"),
       splitby_labels = c("Regular Birthweight", "Low Birthweight"))

}


\keyword{ table1 }
\keyword{ descriptives }
\keyword{ summarize }
\keyword{ formatted }
