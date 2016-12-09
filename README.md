
### CRAN Version (1.2.2)

[![CRAN](http://www.r-pkg.org/badges/version/furniture)](http://www.r-pkg.org/badges/version/furniture)
[![Rdoc](http://www.rdocumentation.org/badges/version/furniture)](http://www.rdocumentation.org/packages/furniture)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/furniture)](http://cranlogs.r-pkg.org/badges/grand-total/furniture)

### Developmental Version (1.4.1)

[![Build Status](https://travis-ci.org/TysonStanley/furniture.svg?branch=master)](https://travis-ci.org/TysonStanley/furniture)
[![codecov](https://codecov.io/gh/tysonstanley/furniture/branch/master/graph/badge.svg)](https://codecov.io/gh/tysonstanley/furniture)

# furniture: 1.4.1

The furniture R package contains functions to help with data cleaning/tidying (e.g., `washer`), exploratory data analysis and reporting (e.g., `table1`, `%xt%`). It currently contains two main functions:

1. `table1()` -- gives a well-formatted table for academic publication of descriptive statistics. Very useful for quick analyses as well. Note that a version of this function, `tableM`, can do missing data analysis in a nice table as well.
2. `washer()` -- changes several values in a variable (very useful for changing place holder values to missing)

In conjunction with many other tidy tools, the package should be useful for health, behavioral, and social scientists working on quantitative research.

# Installation

The latest stable build of the package can be downloaded from CRAN via:

```r
install.packages("furniture")
```
However, currently the CRAN version has a small bug with the percentages. Please use the developmental version until the CRAN version is updated past 1.2.2. You can download the developmental version via:

```r
library(devtools)
install_github("tysonstanley/furniture")
```


# Using furniture

The package is most useful in conjunction with other tidy tools to get data cleaned/tidied and start exploratory data analysis. I recommend using packages such as `library(dplyr)`, `library(tidyr)`, and `library(ggplot2)` with `library(furniture)` to accomplish this.



