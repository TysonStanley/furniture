\name{print.tp}
\alias{print.tp}
\title{Print Method for Two Part Models}
\description{
The print method for two part model objects of class "tp". 
}
\usage{
\method{print}{tp}(x, ...)
}
\arguments{
  \item{x}{
the "tp" object from the \code{tp()} function.
}
  \item{\dots}{
other arguments that can be applied to the print function
}
}

\value{
Prints the coefficient's table from \code{summary.glm()} for both parts of the model.
}

\author{
Tyson S. Barrett
}

\seealso{
\code{\link{tp}}, \code{\link{summary.tp}}
}

\keyword{ print }
\keyword{ tp }
\keyword{ two-part }