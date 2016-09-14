\name{latex_table1_}
\alias{latex_table1_}
\title{ Latex Version of Table 1}
\description{ Function for producing latex version of table 1. Can be used outside of table 1.}

\usage{
latex_table1_(tab, booktabs = TRUE, align=NULL, caption=NULL)
}

\arguments{
  \item{tab}{
      the data.frame that is to be summarized
}
  \item{booktabs}{
      passed to \code{knitr::kable()}
}
  \item{align}{
      passed to \code{knitr::kable()}
}
  \item{caption}{
     passed to \code{knitr::kable()} 
}

}

\author{Tyson Barrett and Emily Brignone}

\seealso{
\code{\link{print.table1}}
}

\keyword{ table1 }
\keyword{ descriptives }
\keyword{ summarize }
\keyword{ formatted }
