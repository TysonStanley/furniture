\name{table1}
\alias{table1}
\alias{summarization}
\title{Table 1 for Social Scientists}
\description{ Produces a descriptive table, stratified by an optional categorical variable, providing means/frequencies and standard deviations/percentages. It is well-formatted for easy transition to academic article or report.}

\usage{
table1(data, vars, splitby = NULL, splitby_labels = NULL, test = FALSE, test.type = "default", rounding = 3, var.names = NULL, format.output = "full", NAkeep = FALSE, m_label = "Missing")
}

\arguments{
  \item{data}{
      the data.frame that is to be summarized
}
  \item{vars}{
      a vector of variables to be summarized; can be the indices or the column names
}
  \item{splitby}{
      the categorical variable to stratify by; requires \code{levels(splitby)>0}
}
  \item{splitby_labels}{
      allows for custom labels of the splitby levels; must match the number of levels of the splitby variable
}
  \item{test}{
      logical; if set to \code{TRUE} then bivariate tests of significance are performed; requires \code{levels(splitby)>1}; automatically performs the correct test based on variable types and number of levels
}
  \item{test.type}{
       has two options: "default" performs the default tests of significance only; "or" gives unadjusted odds ratios as well based on logistic regression
}
  \item{rounding}{
      the number of digits after the decimal; default is 3
}
  \item{var.names}{
      custom variable names to be printed in the table
}
  \item{format.output}{
      has three options: 1) "full" provides the table with the type of test, test statistic, and the p-value for each variable; 2) "pvalues" provides the table with the p-values; 3) "stars" provides the table with stars indicating significance
}
  \item{NAkeep}{
      when set to \code{TRUE} it also shows how many missing values are in the data by variable
}
  \item{m_label}{
      when \code{NAkeep = TRUE} this provides a label for the missing values in the table
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list is returned with potentially two items:
  \item{Table1 }{A data.frame object with the means/frequencies and standard deviations/percentages.}
  \item{Note }{A simple string vector indicating what the stars in the output signify (only available when \code{format.output = "stars"})}
}

\author{Tyson Barrett and Emily Brignone}
\note{
When using \code{dplyr} and other piping packages, the function works best when the data object is a \code{data.frame}. If it is not, use \code{data = as.data.frame(data)} before using this function.
}

\seealso{
\code{\link{tableX}}
}

\examples{

## The function is currently defined as
function (data, vars, splitby = NULL, splitby_labels = NULL, 
    test = FALSE, test.type = "default", rounding = 3, var.names = NULL, 
    format.output = "full", NAkeep = FALSE, m_label = "Missing") 
{
    if (NAkeep) 
        NAkeep = "always"
    else NAkeep = "no"
    if (is.null(splitby)) {
        d = as.data.frame(data[, vars])
        if (!is.null(var.names)) {
            stopifnot(length(var.names) == (length(names(d))))
            names(d) = var.names
        }
        else {
            names(d) = names(data[, vars])
        }
        tab = tab2 = tests = nams = list()
        for (i in 1:dim(d)[2]) {
            nams[[i]] = names(d)[i]
            if (is.factor(d[, i]) | is.character(d[, i])) {
                d[, i] = as.factor(d[, i])
                tab[[i]] = table(d[, i], useNA = NAkeep)
                tab2[[i]] = round(table(d[, i], useNA = NAkeep)/sum(table(d[, 
                  i], useNA = NAkeep)), rounding)
            }
            else if (is.numeric(d[, i]) | is.integer(d[, i])) {
                tab[[i]] = round(mean(d[, i], na.rm = TRUE), 
                  rounding)
                tab2[[i]] = round(sd(d[, i], na.rm = TRUE), rounding)
            }
            else {
                paste("Variables need to be either factor or numeric.")
            }
        }
        tabZ = data.frame(matrix(nrow = length(names(table(d[, 
            1], useNA = NAkeep))), ncol = 2))
        for (j in 1:length(tab)) {
            if (is.factor(d[, j])) {
                tabX = data.frame(names(table(d[, j], useNA = NAkeep)))
            }
            else if (is.numeric(d[, j])) {
                tabX = data.frame(paste(" "))
            }
            if (is.factor(d[, j])) {
                tabX = data.frame(tabX, paste0(round(tab[[j]], 
                  2), " (", round(tab2[[j]] * 100, 1), "\%)"))
            }
            else if (is.numeric(d[, j]) | is.integer(d[, j])) {
                tabX = data.frame(tabX, paste0(round(tab[[j]][[1]], 
                  2), " (", round(tab2[[j]][[1]], 2), ")"))
            }
            n3 = data.frame(names(d)[j], matrix(" ", ncol = 1, 
                nrow = 1))
            names(tabZ) = names(tabX) = names(n3) = c(" ", "Means (SD)")
            tabW = rbind(n3, tabX)
            tabZ = rbind(tabZ, tabW)
        }
        N = data.frame("Observations", dim(d)[1])
        names(N) = c(" ", "Means (SD)")
        rem = ifelse(is.na(tabZ[, 2]), FALSE, TRUE)
        final = tabZ[rem, ]
        final$` ` = as.character(final$` `)
        final$` `[is.na(final$` `)] = m_label
        final = rbind(N, final)
        return(list(Table1 = final))
    }
    else if (!is.null(splitby)) {
        d = as.data.frame(data[, vars])
        if (!is.null(var.names)) {
            stopifnot(length(var.names) == length(names(d)))
            names(d) = var.names
        }
        else {
            names(d) = names(data[, vars])
        }
        N = t(tapply(d[, 1], data[, splitby], length))
        d$split = as.factor(data[, splitby])
        if (!is.null(splitby_labels)) 
            levels(d$split) = splitby_labels
        tab = tab2 = tests = tests2 = nams = list()
        for (i in 1:(dim(d)[2] - 1)) {
            nams[[i]] = names(d)[i]
            if (is.factor(d[, i])) {
                tab[[i]] = tapply(d[, i], d$split, table, useNA = NAkeep)
                tab2[[i]] = tapply(d[, i], d$split, function(x) round(table(x, 
                  useNA = NAkeep)/sum(table(x, useNA = NAkeep)), 
                  rounding))
                if (test) 
                  tests[[i]] = chisq.test(d$split, d[, i])
                if (test & test.type == "or") 
                  tests2[[i]] = glm(d$split ~ d[, i], family = binomial(link = "logit"))
            }
            else if (is.numeric(d[, i]) | is.integer(d[, i])) {
                tab[[i]] = round(tapply(d[, i], d$split, mean, 
                  na.rm = TRUE), rounding)
                tab2[[i]] = round(tapply(d[, i], d$split, sd, 
                  na.rm = TRUE), rounding)
                if (length(levels(d$split)) > 2 & test) {
                  lt = car::leveneTest(y = d[, i], group = d$split)$`Pr(>F)`[1]
                  if (lt < 0.05) {
                    tests[[i]] = oneway.test(d[, i] ~ d$split, 
                      var.equal = FALSE)
                  }
                  else {
                    tests[[i]] = oneway.test(d[, i] ~ d$split, 
                      var.equal = TRUE)
                  }
                }
                else if (test) {
                  tests[[i]] = t.test(d[, i] ~ d$split)
                }
                if (test & test.type == "or") {
                  tests2[[i]] = glm(d$split ~ d[, i], family = binomial(link = "logit"))
                }
            }
            else {
                paste("Variables need to be either factor or numeric.")
            }
        }
        if (test) {
            if (test.type == "or") {
                OR = data.frame(matrix(nrow = length(levels(d[, 
                  i])) + 1, ncol = 4))
                names(OR) = c(" ", "OR", "Lower", "Upper")
            }
            if (format.output == "full") 
                tabZ = data.frame(matrix(nrow = length(levels(d[, 
                  i])), ncol = length(levels(d$split)) + 3))
            else if (format.output == "pvalue" | format.output == 
                "stars") 
                tabZ = data.frame(matrix(nrow = length(levels(d[, 
                  i])), ncol = length(levels(d$split)) + 2))
        }
        else {
            tabZ = data.frame(matrix(nrow = length(levels(d[, 
                i])), ncol = length(levels(d$split)) + 1))
        }
        for (j in 1:length(tab)) {
            if (is.factor(d[, j])) {
                tabX = data.frame(names(table(d[, j], useNA = NAkeep)))
            }
            else if (is.numeric(d[, j])) {
                tabX = data.frame(paste(" "))
            }
            for (i in 1:length(levels(d$split))) {
                if (is.factor(d[, j])) {
                  tabX = data.frame(tabX, paste0(round(tab[[j]][[i]], 
                    2), " (", round(tab2[[j]][[i]] * 100, 1), 
                    "\%)"))
                }
                else if (is.numeric(d[, j])) {
                  tabX = data.frame(tabX, paste0(round(tab[[j]][[i]], 
                    2), " (", round(tab2[[j]][[i]], 2), ")"))
                }
            }
            if (test & test.type == "or" & NAkeep == "no") {
                cis = exp(confint(tests2[[j]]))
                or = exp(tests2[[j]]$coef)
                if (is.numeric(d[, j])) {
                  n4 = data.frame("", round(or[-1], 2), round(cis[-1, 
                    1], 2), round(cis[-1, 2], 2))
                }
                else if (is.factor(d[, j])) {
                  n4 = data.frame("", c(1, round(or[-1], 2)), 
                    c(1, round(cis[-1, 1], 2)), c(1, round(cis[-1, 
                      2], 2)))
                }
                tabQ = data.frame("", "", "", "")
                tabQ[] = sapply(tabQ, as.character)
                names(n4) = names(tabQ) = c(" ", "OR", "Lower", 
                  "Upper")
                n5 = rbind(tabQ, n4)
                OR = rbind(OR, n5)
                rem2 = ifelse(is.na(OR[, 1]), FALSE, TRUE)
                OR = OR[rem2, ]
            }
            else {
                OR = NULL
            }
            if (test & format.output == "full") {
                if (is.factor(d[, j])) {
                  n3 = data.frame(names(d)[j], matrix(" ", ncol = length(levels(d$split)), 
                    nrow = 1), paste("Chi Square:", round(tests[[j]]$statistic, 
                    2)), paste(round(tests[[j]]$p.value, 3)))
                }
                else if (is.numeric(d[, j])) {
                  if (length(levels(d$split)) > 2) {
                    n3 = data.frame(names(d)[j], matrix(" ", 
                      ncol = length(levels(d$split)), nrow = 1), 
                      paste("F-Value:", round(tests[[j]]$statistic[[1]], 
                        2)), paste(round(tests[[j]]$p.value[1], 
                        3)))
                  }
                  else {
                    n3 = data.frame(names(d)[j], matrix(" ", 
                      ncol = length(levels(d$split)), nrow = 1), 
                      paste("T-Test:", round(tests[[j]]$statistic[[1]], 
                        2)), paste(round(tests[[j]]$p.value, 
                        3)))
                  }
                }
                tabX = data.frame(tabX, "", "")
                names(tabZ) = names(tabX) = names(n3) = c(" ", 
                  levels(d$split), "Test", "P-Value")
                tabW = rbind(n3, tabX)
                tabZ = rbind(tabZ, tabW)
            }
            else if (test & format.output == "pvalue") {
                if (is.factor(d[, j])) {
                  n3 = data.frame(names(d)[j], matrix(" ", ncol = length(levels(d$split)), 
                    nrow = 1), paste(round(tests[[j]]$p.value, 
                    3)))
                }
                else if (is.numeric(d[, j])) {
                  if (length(levels(d$split)) > 2) {
                    n3 = data.frame(names(d)[j], matrix(" ", 
                      ncol = length(levels(d$split)), nrow = 1), 
                      paste(round(tests[[j]]$p.value[1], 3)))
                  }
                  else {
                    n3 = data.frame(names(d)[j], matrix(" ", 
                      ncol = length(levels(d$split)), nrow = 1), 
                      paste(round(tests[[j]]$p.value, 3)))
                  }
                }
                tabX = data.frame(tabX, "")
                names(tabZ) = names(tabX) = names(n3) = c(" ", 
                  levels(d$split), "P-Value")
                tabW = rbind(n3, tabX)
                tabZ = rbind(tabZ, tabW)
            }
            else if (test & format.output == "stars") {
                n3 = data.frame(names(d)[j], matrix(" ", ncol = length(levels(d$split)), 
                  nrow = 1), paste(ifelse(tests[[j]]$p.value < 
                  0.001, "***", ifelse(tests[[j]]$p.value < 0.01, 
                  "**", ifelse(tests[[j]]$p.value < 0.05, "*", 
                    "")))))
                tabX = data.frame(tabX, "")
                names(tabZ) = names(tabX) = names(n3) = c(" ", 
                  levels(d$split), "")
                tabW = rbind(n3, tabX)
                tabZ = rbind(tabZ, tabW)
            }
            else {
                n3 = data.frame(names(d)[j], matrix(" ", ncol = length(levels(d$split)), 
                  nrow = 1))
                names(tabZ) = names(tabX) = names(n3) = c(" ", 
                  levels(d$split))
                tabW = rbind(n3, tabX)
                tabZ = rbind(tabZ, tabW)
            }
        }
        if (format.output == "full" & test) {
            N = data.frame("Observations", N, "", "")
            names(N) = c(" ", levels(d$split), "Test", "P-Value")
        }
        else if ((format.output == "pvalues" | format.output == 
            "stars") & test) {
            N = data.frame("Observations", N, "")
            if (format.output == "pvalues") 
                names(N) = c(" ", levels(d$split), "P-Value")
            else names(N) = c(" ", levels(d$split), "")
        }
        else {
            N = data.frame("Observations", N)
            names(N) = c(" ", levels(d$split))
        }
        tabZ = rbind(N, tabZ)
        rem = ifelse(is.na(tabZ[, 2]), FALSE, TRUE)
        final = tabZ[rem, ]
        if (!is.null(OR)) {
            OR = rbind(tabQ, OR)
            final = cbind(final, OR)
            names(final)[4] = " "
        }
        final$` ` = as.character(final$` `)
        final$` `[is.na(final$` `)] = m_label
        if (format.output == "stars") {
            noted = paste("Note: p<.05 = *, p<.01 = **, p<.001 ***")
            return(list(Table1 = final, Note = noted))
        }
        else return(list(Table1 = final))
    }
  }
  
## Data from MASS package ##
library(MASS)
data("birthwt")

## Using dplyr
library(dplyr)
b = mutate(.data=birthwt,
           smoke = as.factor(smoke),
           race  = as.factor(race),
           ht    = as.factor(ht),
           ui    = as.factor(ui))
levels(b$race) = c("white", "black", "other")

## Table1 examples
table1(b, c("age", "race", "smoke", "ptl", "ht", "ui", "ftv"),
       NAkeep=TRUE)

table1(b, c("age", "race", "smoke", "ptl", "ht", "ui", "ftv"),
       splitby="low",
       NAkeep=TRUE)

table1(b, c("age", "race", "smoke", "ptl", "ht", "ui", "ftv"),
       splitby="low",
       test=TRUE)

table1(b, c("age", "race", "smoke", "ptl", "ht", "ui", "ftv"),
       splitby="low",
       test=TRUE,
       var.names = c("Age", "Race", "Smoking Status", "Previous Premature Labors", "Hypertension",
                     "Uterine Irratibility", "Physician Visits"),
       splitby_labels = c("Regular Birthweight", "Low Birthweight"))

table1(b, c("age", "race", "smoke", "ptl", "ht", "ui", "ftv"),
       splitby="low",
       test=TRUE,
       var.names = c("Age", "Race", "Smoking Status", "Previous Premature Labors", "Hypertension",
                     "Uterine Irratibility", "Physician Visits"),
       splitby_labels = c("Regular Birthweight", "Low Birthweight"),
       test.type = "or",
       format.output = "stars")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ table1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ descriptives }% __ONLY ONE__ keyword per line
\keyword{ summarize }
\keyword{ formatted }
