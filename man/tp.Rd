\name{tp}
\alias{tp}
\title{Two-Part Generalized Linear Models}
\description{
Produces a two-part model (sometimes refered to as a "Hurdle Model"). The function is built just like regular \code{glm()} but needs additional specification of what link function to use for the binary part and the family and link to use for the count/OLS part. This procedure is very useful for outcome data that have a large number of zero's (more than would be expected via most distributions). Therefore, it is assumed that two mechanisms are at play: 1) that affects whether an individual has any of the outcome vs. none, and 2) for those that do have some of the outcome, that affects how much. It is often useful for cost and utilization analyses.
}
\usage{
tp(formula, data, bin.link = "logit", count.family = poisson(link = "log"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
the formula in general form (i.e., y ~ x1 + x2 + ...)
}
  \item{data}{
the data for the modeling
}
  \item{bin.link}{
the binary link function. Can be either "logit" or "probit"
}
  \item{count.family}{
the family for the count portion of the models (e.g., \code{poisson(link = "log")})
}
}
\details{
The function is built on the base function \code{glm()} and can therefore perform the types of models inherent in that function.
}
\value{
A list of class "tp" is returned:
\item{Binary }{ the binary portion of the model (distribution of "binomial" with specified link) with all of the data }
\item{Count  }{ the count portion of the model (specified distribution and link) fit to only the data where y > 0}
}
\note{
\code{tp} objects have two specialized methods -- \code{print} and \code{summary}.
}
\references{
http://www.stata-journal.com/article.html?article=st0368
}
\author{
Tyson S. Barrett
}

\seealso{
\code{\link{tp2frames}}
}
\examples{

library(MASS)
library(dplyr)
data(bacteria)

## Getting lengths for sampling
len1 = length(bacteria[bacteria$y=="y", 1])
len2 = length(bacteria[,1])

## Arbitrarily changing data to fit a two-part model
d = transmute(bacteria,
  outcome = ifelse(y == "y", sample(1:100, replace=TRUE, size = len1), 0),
  gender  = as.factor(sample(c("Male", "Female"), replace=TRUE, size = len2)),
  age     = as.numeric(sample(1:18, replace=TRUE, size = length(bacteria[,1]))),
  race    = as.factor(sample(c("White", "Black", "Other"), replace=TRUE, size = len2)),
  weight  = as.numeric(sample(30:90, replace=TRUE, size = len2)))
  
btp = tp(outcome ~ gender + age + race + weight, data=d)
summary.tp(btp)
}

\keyword{ two-part }
\keyword{ hurdle }
\keyword{ glm }