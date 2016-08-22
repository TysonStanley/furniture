\name{summary.tp}
\alias{summary.tp}
\title{
Summary Method for tp Objects
}
\description{
The summary method for objects produced via the \code{tp()} function. Prints the model summaries and the combined average marginal effects.
}
\usage{
\method{summary}{tp}(object, ...)
}

\arguments{
  \item{object}{
the "tp" object
}
  \item{\dots}{
other arguments that can be applied to the summary function
}
}

\value{
Prints the model summaries from \code{summary.glm()} for both parts of the model and the combined average marginal effects.
}

\author{
Tyson S. Barrett
}

\seealso{
\code{\link{tp}}
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
\keyword{ summary }
