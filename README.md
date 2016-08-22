[![Build Status](https://travis-ci.org/TysonStanley/furniture.svg?branch=master)](https://travis-ci.org/TysonStanley/furniture)

# furniture

The furniture R package contains functions to help with data cleaning/tidying (e.g., washer, mirror), exploratory data analysis (e.g., table1, tableX), and modeling (e.g., tp, frames). It currently contains seven main functions:

1. `table1()` -- gives a well-formatted table for academic publication of descriptive statistics. Very useful for quick analyses as well.
2. `tableX()` -- gives model summaries of several "lm" and "glm" models simultaneously
3. `frames()` -- produces average marginal effects of glm models
4. `tp()` -- performs a two-part model for zero inflated count data
5. `tp2frames()` -- takes a tp object and gives the combined average marginal effects
6. `mirror()` -- reverse codes a variable
7. `washer()` -- changes several values in a variable (very useful for changing place holder values to missing)

In conjunction with many other tidy tools, the package should be useful for health, behavioral, and social scientists working on quantitative research.

# Installation

Currently, the package is under the submission process at CRAN. Therefore, the latest available package version can be installed in R via:

```rstudio
library(devtools)
install_github("tysonstanley/furniture")
```

# Using furniture

The package is most useful in conjunction with other tidy tools to get data cleaned/tidied and start exploratory data analysis. I recommend using packages such as `library(dplyr)`, `library(tidyr)`, and `library(ggplot2)` with `library(furniture)` to accomplish this.



