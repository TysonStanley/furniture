\name{table1}
\alias{table1}
\alias{summarization}
\title{Table 1 for Social Scientists}
\description{ Produces a descriptive table, stratified by an optional categorical variable, providing means/frequencies and standard deviations/percentages. It is well-formatted for easy transition to academic article or report.}

\usage{
table1(data, vars, splitby = NULL, splitby_labels = NULL, test = FALSE, test.type = "default", rounding = 3, var.names = NULL, format.output = "full", NAkeep = FALSE, m_label = "Missing")
}

\arguments{
  \item{data}{
      the data.frame that is to be summarized
}
  \item{vars}{
      a vector of variables to be summarized; can be the indices or the column names
}
  \item{splitby}{
      the categorical variable to stratify by; requires \code{levels(splitby)>0}
}
  \item{splitby_labels}{
      allows for custom labels of the splitby levels; must match the number of levels of the splitby variable
}
  \item{test}{
      logical; if set to \code{TRUE} then bivariate tests of significance are performed; requires \code{levels(splitby)>1}; automatically performs the correct test based on variable types and number of levels
}
  \item{test.type}{
       has two options: "default" performs the default tests of significance only; "or" gives unadjusted odds ratios as well based on logistic regression
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
  \item{NAkeep}{
      when set to \code{TRUE} it also shows how many missing values are in the data by variable
}
  \item{m_label}{
      when \code{NAkeep = TRUE} this provides a label for the missing values in the table
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list is returned with potentially two items:
  \item{Table1 }{A data.frame object with the means/frequencies and standard deviations/percentages.}
  \item{Note }{A simple string vector indicating what the stars in the output signify (only available when \code{format.output = "stars"})}
}

\author{Tyson Barrett and Emily Brignone}
\note{
When using \code{dplyr} and other piping packages, the function works best when the data object is a \code{data.frame}. If it is not, use \code{data = as.data.frame(data)} before using this function.
}

\seealso{
\code{\link{tableX}}
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
table1(b, c("age", "race", "smoke", "ptl", "ht", "ui", "ftv"),
       NAkeep=TRUE)

table1(b, c("age", "race", "smoke", "ptl", "ht", "ui", "ftv"),
       splitby="low",
       NAkeep=TRUE)

table1(b, c("age", "race", "smoke", "ptl", "ht", "ui", "ftv"),
       splitby="low",
       test=TRUE)

table1(b, c("age", "race", "smoke", "ptl", "ht", "ui", "ftv"),
       splitby="low",
       test=TRUE,
       var.names = c("Age", "Race", "Smoking Status", "Previous Premature Labors", "Hypertension",
                     "Uterine Irratibility", "Physician Visits"),
       splitby_labels = c("Regular Birthweight", "Low Birthweight"))

table1(b, c("age", "race", "smoke", "ptl", "ht", "ui", "ftv"),
       splitby="low",
       test=TRUE,
       var.names = c("Age", "Race", "Smoking Status", "Previous Premature Labors", "Hypertension",
                     "Uterine Irratibility", "Physician Visits"),
       splitby_labels = c("Regular Birthweight", "Low Birthweight"),
       test.type = "or",
       format.output = "stars")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ descriptives }% __ONLY ONE__ keyword per line
\keyword{ summarize }
\keyword{ formatted }
