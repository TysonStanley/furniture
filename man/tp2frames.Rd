\name{tp2frames}
\alias{tp2frames}
\title{Average Marginal Effects for Two-Part Models}

\description{
Produces the average marginal effects for two-part models (sometimes refered to as "Hurdle Models"). Part 1 is a binomial GLM of "probit" or "logit" links. Part 2 is often a count model (e.g., poisson or gamma) with a "log" or "identity" link.  It combines the marginal effects from both models. Useful to produce the marginal effects of variables in the outcome's original metric (e.g., dollars, counts, etc.). 
}
\usage{
tp2frames(model, bootsize = 1000, ci = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
the model object of class "tp"
}
  \item{bootsize}{
     (default = 1,000) the number of bootstrap samples used for the confidence intervals
}
  \item{ci}{
     (default = 0.95) the confidence level for the confidence intervals
}
}

\value{
A list is returned of the type "frames" with:
\item{AME}{a data.frame of the average marginal effects}
\item{Model }{the output of \code{summary.glm(model)} (e.g., coefficients, standard errors, p-values, call, etc.)}
\item{Variables }{A vector of the variables in the model. Includes the intercept term.}
\item{Family}{ The family distribution and link function}
\item{Bootsize}{ the number of bootstrap samples used for the confidence intervals}
\item{Alpha}{ the alpha level for the confidence intervals (1 - \code{ci})}
\item{Data}{ the data.frame }
}
\references{
http://www.stata.com/help.cgi?margins
}
\author{
Tyson S. Barrett
}
\note{
The function only works with objects produced from the \code{tp()} function in the \code{furniture} package.
}


\seealso{
\code{\link{frames}}, \code{\link{tp}}
}
\examples{

library(MASS)
library(dplyr)
data(bacteria)
d = transmute(bacteria,
           outcome = ifelse(y == "y", sample(1:100, replace=TRUE, size = length(bacteria[bacteria$y=="y",1])), 0),
           gender  = as.factor(sample(c("Male", "Female"), replace=TRUE, size = length(bacteria[,1]))),
           age     = as.numeric(sample(1:18, replace=TRUE, size = length(bacteria[,1]))),
           race    = as.factor(sample(c("White", "Black", "Other"), replace=TRUE, size = length(bacteria[,1]))),
           weight  = as.numeric(sample(30:90, replace=TRUE, size = length(bacteria[,1]))))
  
btp = tp(outcome ~ gender + age + race + weight, data=d)
summary(btp)
}

\keyword{ average }
\keyword{ marginal }
\keyword{ effects }
\keyword{ glm }
\keyword{ non-linear }
\keyword{ two-part }