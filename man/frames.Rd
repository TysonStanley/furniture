\name{frames}
\alias{frames}
\title{Average Marginal Effects for Generalized Linear Models}
\description{
Produces the average marginal effects for generalized linear models with links of "probit", "logit", "log", and "identity". Useful to produce the marginal effects of variables in the outcome's original metric (e.g., dollars, counts, etc.). The function can take a model already run (as a "glm" object) or can run the model with the formula and family.
}
\usage{
frames(model, bootsize = 100, ci = 0.95)
}
\arguments{
  \item{model}{
     the model object (i.e., "glm")
}
  \item{bootsize}{
     (default = 100) the number of bootstrap samples used for the confidence intervals
}
  \item{ci}{
     (default = 0.95) the confidence level for the confidence intervals
}
}
\details{
Uses bootstrapping to get the confidence intervals. Marginal effects are produced using the derivatives of the variables. 
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
http://www.stata-journal.com/sjpdf.html?articlenum=st0086
}
\author{
Tyson S. Barrett
}
\note{
The \code{frames()} function is still in early development. The results will be similar to Stata's margin command but the exact values and confidence intervals will likely be somewhat different. 

This function has two specialized methods -- \code{print} and \code{summary}.

Note: Average marginal effects for interactions may not be correctly computed. This is an area of future development.
}

\seealso{
\code{\link{tp2frames}}
}

\examples{

library(MASS)
data(birthwt)

for (i in c(1,4:5,7:8)){
  birthwt[,i] = as.factor(birthwt[,i])
}

logglm   = glm(bwt ~ smoke + age + lwt + race + ht, data=birthwt, family = Gamma(link = "log"))
lgtglm   = glm(low ~ smoke + age + lwt + race + ht, data=birthwt, family = "binomial")
myglm    = glm(bwt ~ smoke + age + lwt + race + ht, data=birthwt)

log = frames(logglm, bootsize = 100)
lgt = frames(lgtglm, bootsize = 100)
lin = frames(myglm, bootsize = 100)
}

\keyword{ average }
\keyword{ marginal }
\keyword{ effects }
\keyword{ glm }
\keyword{ non-linear }