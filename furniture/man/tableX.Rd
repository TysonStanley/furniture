\name{tableX}
\alias{tableX}

\title{Table X for Social Scientists}
\description{
Provides a summary of n > 1 linear models in a well-formatted table, ready for export into a social science academic article or report.
}
\usage{
tableX(models, model.names, type = "default", level = 0.95)
}
\arguments{
  \item{models}{
      a list of the model objects; can be "lm" or "glm" objects
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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Outputs a list with two elements: 
    \item{Table }{the table of the model summary}
    \item{Note  }{indicates the level of significance that the stars signify; only useful for when \code{type = "default"}}
}

\author{
Tyson Barrett
}
\note{
As of now, only "lm" and "glm" objects are able to be included. This will be updated with future releases.
}

\seealso{
\code{\link{table1}}
}
\examples{

## The function is currently defined as
function (models, model.names, type = "default", level = 0.95) 
{
    if (type != "default" & type != "exp") 
        stop(cat(paste("The", type, "method is not supported. Yet. Let me know you want it @ t.barrett@aggiemail.usu.edu.", 
            "\n", "Types Supported: default (reports estimates and standard errors) \n                   and exp (reports exponentiated estimates and confidence intervals.")))
    if (!all(sapply(models, class) == "lm" | sapply(models, class) == 
        "glm")) 
        stop(cat(paste("The models must be 'lm' or 'glm' objects. If there is another modeling type \n                   let me know you want it @ t.barrett@aggiemail.usu.edu.")))
    m = m2 = rows = pval = se = obs = N = list()
    for (i in seq_along(models)) {
        summed = summary(models[[i]])$coefficients
        m[[i]] = data.frame(Est = summed[, 1])
        se[[i]] = data.frame(SE = summed[, 2])
        pval[[i]] = data.frame(PValue = summed[, 4])
        obs[[i]] = data.frame(Est = sum(!is.na(models[[i]]$fitted.values)))
    }
    if (type == "default") {
        for (i in seq_along(m)) {
            m2[[i]] = data.frame(Est = round(m[[i]], 2), SE = round(se[[i]], 
                2), Sig = paste(ifelse(pval[[i]] < 0.001, "***", 
                ifelse(pval[[i]] < 0.01, "**", ifelse(pval[[i]] < 
                  0.05, "*", "")))), names = rownames(m[[i]]))
            N[[i]] = data.frame(Est = obs[[i]], SE = NA, Sig = NA, 
                names = "N")
            m2[[i]] = rbind(N[[i]], m2[[i]])
        }
    }
    if (type == "exp") {
        for (i in seq_along(m)) {
            cis = exp(confint(models[[i]], alpha = 1 - level))
            m2[[i]] = data.frame(Est = round(exp(m[[i]][, "Est"]), 
                3), Lower = round(cis[, 1], 2), Upper = round(cis[, 
                2], 2), names = rownames(m[[i]]))
            N[[i]] = data.frame(Est = obs[[i]], Lower = NA, Upper = NA, 
                names = "N")
            m2[[i]] = rbind(N[[i]], m2[[i]])
        }
    }
    options(warn = -1)
    merged = Reduce(function(...) merge(..., by = "names", all = TRUE), 
        m2)
    names(merged) = c("Variables", rep(model.names, each = 3))
    merged[, c(seq(3, length.out = length(model.names), by = 3))] <- sapply(merged[, 
        c(seq(3, length.out = length(model.names), by = 3))], 
        as.character)
    merged[is.na(merged)] <- ""
    Note = paste("Sig Levels: *** < 0.001, ** < 0.01, * < 0.05")
    return(list(Table = merged, Note = Note))
  }
  
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ descriptives }
\keyword{ summarize }



