[![Build Status](http://www.r-pkg.org/badges/version/furniture)](http://www.r-pkg.org/badges/version/furniture)
[![Build Status](https://travis-ci.org/TysonStanley/furniture.svg?branch=master)](https://travis-ci.org/TysonStanley/furniture)



# furniture

The furniture R package contains functions to help with data cleaning/tidying (e.g., washer, mirror), exploratory data analysis and reporting (e.g., table1). It currently contains two main functions:

1. `table1()` -- gives a well-formatted table for academic publication of descriptive statistics. Very useful for quick analyses as well.
2. `washer()` -- changes several values in a variable (very useful for changing place holder values to missing)

In conjunction with many other tidy tools, the package should be useful for health, behavioral, and social scientists working on quantitative research.

# Installation

Currently, the package is under the submission process at CRAN. Therefore, the latest available package version can be installed in R via:

```rstudio
library(devtools)
install_github("tysonstanley/furniture")
```

# Using furniture

The package is most useful in conjunction with other tidy tools to get data cleaned/tidied and start exploratory data analysis. I recommend using packages such as `library(dplyr)`, `library(tidyr)`, and `library(ggplot2)` with `library(furniture)` to accomplish this.



