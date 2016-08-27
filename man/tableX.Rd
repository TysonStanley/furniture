\name{tableX}
\alias{tableX}

\title{Table X for Social Scientists}
\description{
Provides a summary of n > 1 linear models in a well-formatted table, ready for export into a social science academic article or report.
}
\usage{
tableX(..., model.names, type = "default", level = 0.95)
}
\arguments{
  \item{\dots}{
      the model objects; can be "lm" or "glm" objects
}
  \item{model.names}{
      a vector of character strings that will name each model
}
  \item{type}{
      has two options: 1) "default" produces the coefficients, standard errors, and stars indicating significance; 2) "exp" provides exponentiated coefficients and expontiated confidence intervals
}
  \item{level}{
      the level of confidence in the confidence intervals when \code{type = "exp"}.
}
}

\value{
Outputs a list with two elements: 
    \item{Table }{the table of the model summary}
    \item{Note  }{indicates the level of significance that the stars signify; only useful for when \code{type = "default"}}
}

\author{Tyson Barrett}
\note{
As of now, only "lm" and "glm" objects are able to be included. This will be updated with future releases.
}

\seealso{
\code{\link{table1}}
}
\examples{

## Data from MASS package ##
library(MASS)
data("birthwt")

## Modeling Linear Regression ##
fit1 = lm(bwt ~ age + race, data=birthwt)
fit2 = lm(bwt ~ age + race + ht + ui + smoke + ptl, data=birthwt)
fit3 = lm(bwt ~ age + race + ht + ui + smoke + ptl + ftv, data=birthwt)

## TableX ##
models = list(fit1, fit2, fit3)
tableX(models=models, model.names = c("1", "2", "3"))

## Modeling Logistic Regression ##
log1 = glm(low ~ age + race, data=birthwt, family="binomial")
log2 = glm(low ~ age + race + ht + ui + smoke + ptl, data=birthwt, family="binomial")
log3 = glm(low ~ age + race + ht + ui + smoke + ptl + ftv, data=birthwt, family="binomial")

## TableX ##
models = list(log1, log2, log3)
tableX(models=models, model.names = c("1", "2", "3"), type="exp")

}


\keyword{ tableX }
\keyword{ descriptives }
\keyword{ summarize }



