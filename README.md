
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version/furniture)](https://cran.r-project.org/package=furniture)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/furniture)
[![Build
Status](https://travis-ci.org/TysonStanley/furniture.svg?branch=master)](https://travis-ci.org/TysonStanley/furniture)

# furniture: 1.8.5 <img src="man/figures/furniture_hex_v2_full.png" align="right" width="40%" height="40%" />

The furniture R package contains functions to help with data
cleaning/tidying (e.g., `washer()`, `rowmeans()`, `rowsums()`),
exploratory data analysis and reporting (e.g., `table1()`, `tableC()`,
`tableF()`). It currently contains eight main functions:

1.  `table1()` – gives a well-formatted table for academic publication
    of descriptive statistics. Very useful for quick analyses as well.
    Notably, `table1()` now works with `dplyr::group_by()`.
2.  `tableC()` – gives a well-formatted table of correlations.
3.  `tableF()` – provides a thorough frequency table for quick checks of
    the levels of a variable.
4.  `washer()` – changes several values in a variable (very useful for
    changing place holder values to missing).
5.  `long()` – is a wrapper of `stats::reshape()`, takes the data from
    wide to long format (long is often the tidy version of the data),
    works well with the tidyverse, and can handle unbalanced multilevel
    data.
6.  `wide()` – also a wrapper of `stats::reshape()`, takes the data from
    long to wide, and like `long()`, works well with the tidyverse and
    can handle unbalanced multilevel data.
7.  `rowmeans()` and `mutate_rowmeans()` – tidyverse friendly versions
    of `rowMeans()`
8.  `rowsums()` and `mutate_rowsums()` – tidyverse friendly versions of
    `rowSums()`

In conjunction with many other tidy tools, the package should be useful
for health, behavioral, and social scientists working on quantitative
research.

# Installation

The latest stable build of the package can be downloaded from CRAN via:

``` r
install.packages("furniture")
```

You can download the developmental version via:

``` r
library(devtools)
install_github("tysonstanley/furniture")
```

# Using furniture

The main functions are the `table_()` functions (e.g., `table1()`,
`tableC()`, `tableF()`).

``` r
library(furniture)
```

    #> Loading furniture
    #> ── furniture 1.8.5 ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── learn more at tysonbarrett.com ──
    #> ✔ furniture attached
    #> ✔ No potential conflicts found

``` r
data("nhanes_2010")

table1(nhanes_2010,
       age, marijuana, illicit, rehab,
       splitby=~asthma)
#> Warning in table1.data.frame(nhanes_2010, age, marijuana, illicit, rehab, : Not all variables have at least 2 unique values. Functionality of the following will be limited:
#> 
#>             -- type = 'condense' will not work
#> 
#>             -- test = TRUE will not work
#> 
#> ───────────────────────────────────
#>                   asthma 
#>            Yes         No         
#>            n = 131     n = 583    
#>  age                              
#>            23.2 (3.7)  23.2 (3.9) 
#>  marijuana                        
#>     Yes    131 (100%)  583 (100%) 
#>     No     0 (0%)      0 (0%)     
#>  illicit                          
#>     Yes    23 (17.6%)  117 (20.1%)
#>     No     108 (82.4%) 466 (79.9%)
#>  rehab                            
#>     Yes    10 (7.6%)   37 (6.3%)  
#>     No     121 (92.4%) 546 (93.7%)
#> ───────────────────────────────────
```

``` r
table1(nhanes_2010,
       age, marijuana, illicit, rehab,
       splitby=~asthma, 
       output = "text2")
#> Warning in table1.data.frame(nhanes_2010, age, marijuana, illicit, rehab, : Not all variables have at least 2 unique values. Functionality of the following will be limited:
#> 
#>             -- type = 'condense' will not work
#> 
#>             -- test = TRUE will not work
#> 
#> ───────────────────────────────────
#>                   asthma 
#>            Yes         No         
#>            n = 131     n = 583    
#>  --------- ----------- -----------
#>  age                              
#>            23.2 (3.7)  23.2 (3.9) 
#>  marijuana                        
#>     Yes    131 (100%)  583 (100%) 
#>     No     0 (0%)      0 (0%)     
#>  illicit                          
#>     Yes    23 (17.6%)  117 (20.1%)
#>     No     108 (82.4%) 466 (79.9%)
#>  rehab                            
#>     Yes    10 (7.6%)   37 (6.3%)  
#>     No     121 (92.4%) 546 (93.7%)
#> ───────────────────────────────────
```

``` r
library(tidyverse)
nhanes_2010 %>%
  group_by(asthma) %>%
  table1(age, marijuana, illicit, rehab,
         output = "text2")
#> 
#> ───────────────────────────────────
#>                   asthma 
#>            Yes         No         
#>            n = 131     n = 583    
#>  --------- ----------- -----------
#>  age                              
#>            23.2 (3.7)  23.2 (3.9) 
#>  marijuana                        
#>     Yes    131 (100%)  583 (100%) 
#>     No     0 (0%)      0 (0%)     
#>  illicit                          
#>     Yes    23 (17.6%)  117 (20.1%)
#>     No     108 (82.4%) 466 (79.9%)
#>  rehab                            
#>     Yes    10 (7.6%)   37 (6.3%)  
#>     No     121 (92.4%) 546 (93.7%)
#> ───────────────────────────────────
```

`table1()` can be outputted directly to other formats. All
`knitr::kable()` options are available for this and there is an extra
option `"latex2"` which provides a publication ready table in Latex
documents.

``` r
tableC(nhanes_2010, 
       age, active, vig_active, 
       na.rm=TRUE)
#> N = 317
#> Note: pearson correlation (p-value).
#> 
#> ──────────────────────────────────────────────────
#>                [1]            [2]           [3]  
#>  [1]age        1.00                              
#>  [2]active     -0.148 (0.008) 1.00               
#>  [3]vig_active -0.083 (0.141) 0.828 (<.001) 1.00 
#> ──────────────────────────────────────────────────
```

``` r
tableF(nhanes_2010, age)
#> 
#> ──────────────────────────────────
#>  age Freq CumFreq Percent CumPerc
#>  18  191  191     13.48%  13.48% 
#>  19  153  344     10.80%  24.28% 
#>  20  111  455     7.83%   32.11% 
#>  21  95   550     6.70%   38.81% 
#>  22  100  650     7.06%   45.87% 
#>  23  112  762     7.90%   53.78% 
#>  24  93   855     6.56%   60.34% 
#>  25  100  955     7.06%   67.40% 
#>  26  91   1046    6.42%   73.82% 
#>  27  77   1123    5.43%   79.25% 
#>  28  91   1214    6.42%   85.67% 
#>  29  86   1300    6.07%   91.74% 
#>  30  117  1417    8.26%   100.00%
#> ──────────────────────────────────
```

In addition, the `rowmeans()` and `rowsums()` functions offer a
simplified use of `rowMeans()` and `rowSums()`, particularly when using
the tidyverse’s `mutate()`.

``` r
nhanes_2010 %>%
  select(vig_active, mod_active) %>%
  mutate(avg_active = rowmeans(vig_active, mod_active, na.rm=TRUE)) %>%
  mutate(sum_active = rowsums(vig_active, mod_active, na.rm=TRUE))
#> # A tibble: 1,417 x 4
#>    vig_active mod_active avg_active sum_active
#>         <dbl>      <dbl>      <dbl>      <dbl>
#>  1         30         NA         30         30
#>  2        180        180        180        360
#>  3         NA         NA        NaN          0
#>  4         20         70         45         90
#>  5        120         NA        120        120
#>  6         NA         NA        NaN          0
#>  7         NA        120        120        120
#>  8        120         NA        120        120
#>  9         NA         NA        NaN          0
#> 10         NA         NA        NaN          0
#> # … with 1,407 more rows
```

``` r
nhanes_2010 %>%
  mutate_rowmeans("avg_active", mod_active:vig_active)
#> # A tibble: 1,417 x 3
#>    mod_active vig_active avg_active
#>         <dbl>      <dbl>      <dbl>
#>  1         NA         30         NA
#>  2        180        180        180
#>  3         NA         NA         NA
#>  4         70         20         45
#>  5         NA        120         NA
#>  6         NA         NA         NA
#>  7        120         NA         NA
#>  8         NA        120         NA
#>  9         NA         NA         NA
#> 10         NA         NA         NA
#> # … with 1,407 more rows
```

## Notes

The package is most useful in conjunction with other tidy tools to get
data cleaned/tidied and start exploratory data analysis. I recommend
using packages such as `library(dplyr)`, `library(tidyr)`, and
`library(ggplot2)` with `library(furniture)` to accomplish this.

The most important function–`table1()`–is simply built for both
exploratory descriptive analysis and communication of findings. See
vignettes or [tysonbarrett.com](https://tysonstanley.github.io/) for
several examples of its use. Also see our paper in the [R
Journal](https://journal.r-project.org/archive/2017/RJ-2017-037/RJ-2017-037.pdf).
