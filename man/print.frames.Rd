\name{print.frames}
\alias{print.frames}
\title{Print Method for the Frames Object}
\description{
The print method for the frames object. By default, only the Average Marginal Effects are shown in the output from the frames object.
}
\usage{
\method{print}{frames}(x, ...)
}
\arguments{
  \item{x}{
the "frames" object
}
  \item{\dots}{
other arguments that can be applied to the print function
}
}
\value{
The data.frame of AME from the \code{frames()} function is printed.
}

\author{
Tyson S. Barrett
}

\seealso{
\code{\link{frames}}, \code{\link{tp2frames}}, \code{\link{summary.frames}}
}

\keyword{ print }
\keyword{ frames }
