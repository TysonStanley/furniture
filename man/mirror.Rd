\name{mirror}
\alias{mirror}

\title{Mirror for Reverse Coding}
\description{
The \code{mirror()} function takes a variable, a lower bound for the recode and an upper bound for the recode. It reverses the coding of the original variable by applying the lower bound for the recode to the highest value in the variable, lowest value in the original variable and replaces it with the upper bound value and all the values in between are reverse coded as well.
}
\usage{
mirror(data, variable, lower, upper, replace = FALSE)
}

\arguments{
  \item{data}{
 data.frame object where the variable is taken from
}
  \item{variable}{
 the variable to be reverse coded
}
  \item{lower}{
 lower bound of the replacing coding
}
  \item{upper}{
 upper bound of the replacing coding
}
  \item{replace}{
 (optional) default set to FALSE. If set to TRUE, it replaces the original variable with the new coding; otherwise, it adds a new variable to the data.frame.
}
}
\details{
If the original values to be replaced have 1 - 5 (5 levels) then \code{lower:upper} needs to have 5 levels.
}
\value{
Returns the data.frame with either the original variable replaced with the reverse coded variable (\code{replace=TRUE}) or the data.frame with an additional recoded variable.
}

\author{
Tyson S. Barrett
}


\seealso{
\code{\link{washer}}
}
\examples{
## Variable
d = data.frame("ID"=1:20, "Var"=rep(1:5, 4))
## Reverse coding 1 - 5 to 0 - 4 (5 = 0, 4 = 1, etc.)
mirror(d, "Var", 0, 4)

}

\keyword{ reverse }
\keyword{ code }
