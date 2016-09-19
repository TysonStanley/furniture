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
The furniture package provides functions that are useful for exploratory data analysis, data cleaning, and reporting for the health, behavioral and social sciences.
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

## Load the Package
library(furniture)

## Using dplyr ##
library(dplyr)
b = mutate(.data=birthwt,
           smoke = as.factor(smoke),
           race  = as.factor(race),
           ht    = as.factor(ht),
           ui    = as.factor(ui))
levels(b$race) = c("white", "black", "other")

## Table1 example ##

table1(b, age, race, smoke, ptl, ht, ui, ftv,
       splitby=~factor(low),
       test=TRUE,
       var_names = c("Age", "Race", "Smoking Status", "Previous Premature Labors", "Hypertension",
                     "Uterine Irratibility", "Physician Visits"),
       splitby_labels = c("Regular Birthweight", "Low Birthweight"),
       format_output = "stars")


}
