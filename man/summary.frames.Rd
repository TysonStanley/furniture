\name{summary.frames}
\alias{summary.frames}
\alias{summary}
\title{
Summary Method for Frames Objects
}
\description{
The summary method for objects produced via \code{frames()}. 
}
\usage{
summary(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
the "frames" object
}
  \item{\dots}{
other arguments that can be applied to the summary method
}
}

\value{
Prints the \code{summary.glm(framesobj)} and the average marginal effects
}

\author{
Tyson S. Barrett
}

\seealso{
\code{\link{frames}}, \code{\link{print.frames}}, \code{\link{tp2frames}}
}
\examples{
library(MASS)
data(birthwt)

for (i in c(1,4:5,7:8)){
  birthwt[,i] = as.factor(birthwt[,i])
}

logglm   = glm(bwt ~ smoke + age + lwt + race + ht + ui, data=birthwt, family = Gamma(link = "log"))
lgtglm   = glm(low ~ smoke + age + lwt + race + ht + ui, data=birthwt, family = "binomial")
myglm    = glm(bwt ~ smoke + age + lwt + race + ht + ui, data=birthwt)

log = frames(logglm, bootsize = 100)
lgt = frames(lgtglm, bootsize = 100)
lin = frames(myglm,bootsize = 100)

summary(log)
summary(lgt)
summary(lin)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ average }% use one of  RShowDoc("KEYWORDS")
\keyword{ marginal }% __ONLY ONE__ keyword per line
\keyword{ effects }% use one of  RShowDoc("KEYWORDS")
\keyword{ summary }% __ONLY ONE__ keyword per line