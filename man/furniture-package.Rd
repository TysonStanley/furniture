\name{furniture-package}
\alias{furniture-package}
\alias{furniture}
\docType{package}
\title{ \packageTitle{furniture}}
\description{\packageDescription{furniture}}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{furniture}
\packageIndices{furniture}
The furniture package provides several functions that are useful for exploratory data analysis, data cleaning, and reporting for the health, behavioral and social sciences.
}
\author{
\packageAuthor{furniture}

Maintainer: \packageMaintainer{furniture}
}

\keyword{ tables }
\keyword{ summary }
\keyword{ social }
\keyword{ science }
\keyword{ descriptives }

\examples{
## Data from MASS package ##
library(MASS)
data("birthwt")

## Using dplyr ##
library(dplyr)
b = mutate(.data=birthwt,
           smoke = as.factor(smoke),
           race  = as.factor(race),
           ht    = as.factor(ht),
           ui    = as.factor(ui))
levels(b$race) = c("white", "black", "other")

## Table1 examples ##

table1(b, c("age", "race", "smoke", "ptl", "ht", "ui", "ftv"),
       splitby="low",
       test=TRUE,
       var.names = c("Age", "Race", "Smoking Status", "Previous Premature Labors", "Hypertension",
                     "Uterine Irratibility", "Physician Visits"),
       splitby_labels = c("Regular Birthweight", "Low Birthweight"))

table1(b, age, race, smoke, ptl, ht, ui, ftv,
       splitby=~low,
       test=TRUE,
       var.names = c("Age", "Race", "Smoking Status", "Previous Premature Labors", "Hypertension",
                     "Uterine Irratibility", "Physician Visits"),
       splitby_labels = c("Regular Birthweight", "Low Birthweight"),
       test.type = "or",
       format.output = "stars")


}
