% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/long_wide.R
\name{long}
\alias{long}
\title{Wide to Long Data Reshaping}
\usage{
long(
  data,
  ...,
  v.names = NULL,
  id = NULL,
  timevar = NULL,
  times = NULL,
  sep = ""
)
}
\arguments{
\item{data}{the data.frame containing the wide format data}

\item{...}{the variables that are time-varying that are to be placed in 
long format, needs to be in the format
\code{c("x1", "x2"), c("z1", "z2"), etc.}. If the data is
unbalanced (e.g., there are three time points measured for one variable but
only two for another), using the placeholder variable \code{miss},
helps fix this.}

\item{v.names}{a vector of the names for the newly created variables (length
same as number of vectors in \code{varying})}

\item{id}{the ID variable in quotes}

\item{timevar}{the column with the "time" labels}

\item{times}{the labels of the \code{timevar} (default is numeric)}

\item{sep}{the separating character between the wide format variable names
(default is \code{""}); e.g. "x1" and "x2" would create the variable name of
"x"; only applicable if \code{v.names}}
}
\description{
\code{long()} is a wrapper of \code{stats::reshape()} that takes
the data from a wide format to a long format. It can also handle unbalanced
data (where some measures have different number of "time points").
}
\examples{

x1 <- runif(1000)
x2 <- runif(1000)
x3 <- runif(1000)
y1 <- rnorm(1000)
y2 <- rnorm(1000)
z  <- factor(sample(c(0,1), 1000, replace=TRUE))
a  <- factor(sample(c(1,2), 1000, replace=TRUE))
b  <- factor(sample(c(1,2,3,4), 1000, replace=TRUE))
df  <- data.frame(x1, x2, x3, y1, y2, z, a, b)

## "Balanced" Data
ldf1 <- long(df, 
             c("x1", "x2"), c("y1", "y2"),
             v.names = c("x", "y"))

## "Unbalanced" Data
ldf2 = long(df, 
            c("x1", "x2", "x3"), c("y1", "y2", "miss"),
            v.names = c("x", "y"))


}
\seealso{
\code{stats::reshape()} and \code{sjmisc::to_long()}
}
\author{
Tyson S. Barrett
}
