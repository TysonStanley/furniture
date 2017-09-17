
### CRAN Version (1.6.0)

[![CRAN](http://www.r-pkg.org/badges/version/furniture)](http://www.r-pkg.org/badges/version/furniture)
[![Rdoc](http://www.rdocumentation.org/badges/version/furniture)](http://www.rdocumentation.org/packages/furniture)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/furniture)](http://cranlogs.r-pkg.org/badges/grand-total/furniture)

### Developmental Version (1.6.0)

[![Build Status](https://travis-ci.org/TysonStanley/furniture.svg?branch=master)](https://travis-ci.org/TysonStanley/furniture)
[![codecov](https://codecov.io/gh/tysonstanley/furniture/branch/master/graph/badge.svg)](https://codecov.io/gh/tysonstanley/furniture)

# furniture: 1.6.0

The furniture R package contains functions to help with data cleaning/tidying (e.g., `washer`), exploratory data analysis and reporting (e.g., `table1`, `%xt%`). It currently contains four main functions:

1. `table1()` -- gives a well-formatted table for academic publication of descriptive statistics. Very useful for quick analyses as well. 
2. `washer()` -- changes several values in a variable (very useful for changing place holder values to missing).
3. `long()` -- is a wrapper of `stats::reshape()`, takes the data from wide to long format (long is often the tidy version of the data), works well with the tidyverse, and can handle unbalanced multilevel data.
4. `wide()` -- also a wrapper of `stats::reshape()`, takes the data from long to wide, and like `long()`, works well with the tidyverse and can handle unbalanced multilevel data.
5. `tableC()` -- gives a well-formatted table of correlations.

In conjunction with many other tidy tools, the package should be useful for health, behavioral, and social scientists working on quantitative research.

# Installation

The latest stable build of the package can be downloaded from CRAN via:

```r
install.packages("furniture")
```
You can download the developmental version via:

```r
library(devtools)
install_github("tysonstanley/furniture")
```

# Using furniture

The package is most useful in conjunction with other tidy tools to get data cleaned/tidied and start exploratory data analysis. I recommend using packages such as `library(dplyr)`, `library(tidyr)`, and `library(ggplot2)` with `library(furniture)` to accomplish this.

The most important function--`table1`--is simply built for both exploratory descriptive analysis and communication of findings. See vignettes or [tysonstanley.github.io](https://tysonstanley.github.io/) for several examples of its use. Also see the pre-print of our paper in the [R Journal](https://journal.r-project.org/archive/2017/RJ-2017-037/RJ-2017-037.pdf).

