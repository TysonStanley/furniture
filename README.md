
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version/furniture)](https://cran.r-project.org/package=furniture)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/furniture)
[![Build
Status](https://travis-ci.org/TysonStanley/furniture.svg?branch=master)](https://travis-ci.org/TysonStanley/furniture)

# furniture: 1.9.4 <img src="man/figures/furniture_hex_v2_full.png" align="right" width="40%" height="40%" />

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

The main functions are the `table*()` functions (e.g., `table1()`,
`tableC()`, `tableF()`).

``` r
library(furniture)
```

``` r
data("nhanes_2010")

table1(nhanes_2010,
       age, marijuana, illicit, rehab,
       splitby=~asthma,
       na.rm = FALSE)
#> 
#> ───────────────────────────────────
#>                   asthma 
#>            Yes         No         
#>            n = 251     n = 1164   
#>  age                              
#>            23.0 (3.9)  23.4 (4.0) 
#>  marijuana                        
#>     Yes    131 (52.2%) 584 (50.2%)
#>     No     97 (38.6%)  434 (37.3%)
#>     NA     23 (9.2%)   146 (12.5%)
#>  illicit                          
#>     Yes    23 (9.2%)   117 (10.1%)
#>     No     205 (81.7%) 901 (77.4%)
#>     NA     23 (9.2%)   146 (12.5%)
#>  rehab                            
#>     Yes    10 (4%)     37 (3.2%)  
#>     No     121 (48.2%) 547 (47%)  
#>     NA     120 (47.8%) 580 (49.8%)
#> ───────────────────────────────────
```

``` r
table1(nhanes_2010,
       age, marijuana, illicit, rehab,
       splitby=~asthma, 
       output = "text2",
       na.rm = FALSE)
#> 
#> ───────────────────────────────────
#>                   asthma 
#>            Yes         No         
#>            n = 251     n = 1164   
#>  --------- ----------- -----------
#>  age                              
#>            23.0 (3.9)  23.4 (4.0) 
#>  marijuana                        
#>     Yes    131 (52.2%) 584 (50.2%)
#>     No     97 (38.6%)  434 (37.3%)
#>     NA     23 (9.2%)   146 (12.5%)
#>  illicit                          
#>     Yes    23 (9.2%)   117 (10.1%)
#>     No     205 (81.7%) 901 (77.4%)
#>     NA     23 (9.2%)   146 (12.5%)
#>  rehab                            
#>     Yes    10 (4%)     37 (3.2%)  
#>     No     121 (48.2%) 547 (47%)  
#>     NA     120 (47.8%) 580 (49.8%)
#> ───────────────────────────────────
```

``` r
library(tidyverse)
nhanes_2010 %>%
  group_by(asthma) %>%
  table1(age, marijuana, illicit, rehab,
         output = "text2",
         na.rm = FALSE)
#> 
#> ───────────────────────────────────
#>                   asthma 
#>            Yes         No         
#>            n = 251     n = 1164   
#>  --------- ----------- -----------
#>  age                              
#>            23.0 (3.9)  23.4 (4.0) 
#>  marijuana                        
#>     Yes    131 (52.2%) 584 (50.2%)
#>     No     97 (38.6%)  434 (37.3%)
#>     NA     23 (9.2%)   146 (12.5%)
#>  illicit                          
#>     Yes    23 (9.2%)   117 (10.1%)
#>     No     205 (81.7%) 901 (77.4%)
#>     NA     23 (9.2%)   146 (12.5%)
#>  rehab                            
#>     Yes    10 (4%)     37 (3.2%)  
#>     No     121 (48.2%) 547 (47%)  
#>     NA     120 (47.8%) 580 (49.8%)
#> ───────────────────────────────────
```

`table1()` can do bivariate significance tests as well.

``` r
library(tidyverse)
nhanes_2010 %>%
  group_by(asthma) %>%
  table1(age, marijuana, illicit, rehab,
         output = "text2",
         na.rm = FALSE,
         test = TRUE)
#> 
#> ───────────────────────────────────────────
#>                   asthma 
#>            Yes         No          P-Value
#>            n = 251     n = 1164           
#>  --------- ----------- ----------- -------
#>  age                               0.201  
#>            23.0 (3.9)  23.4 (4.0)         
#>  marijuana                         1      
#>     Yes    131 (52.2%) 584 (50.2%)        
#>     No     97 (38.6%)  434 (37.3%)        
#>     NA     23 (9.2%)   146 (12.5%)        
#>  illicit                           0.623  
#>     Yes    23 (9.2%)   117 (10.1%)        
#>     No     205 (81.7%) 901 (77.4%)        
#>     NA     23 (9.2%)   146 (12.5%)        
#>  rehab                             0.729  
#>     Yes    10 (4%)     37 (3.2%)          
#>     No     121 (48.2%) 547 (47%)          
#>     NA     120 (47.8%) 580 (49.8%)        
#> ───────────────────────────────────────────
```

By default it does the appropriate parametric tests. However, you can
change that by setting `param = FALSE` (new with `v 1.9.1`).

``` r
library(tidyverse)
nhanes_2010 %>%
  group_by(asthma) %>%
  table1(age, marijuana, illicit, rehab,
         output = "text2",
         na.rm = FALSE,
         test = TRUE,
         param = FALSE,
         type = "condense")
#> 
#> ───────────────────────────────────────────────
#>                       asthma 
#>                Yes         No          P-Value
#>                n = 251     n = 1164           
#>  ------------- ----------- ----------- -------
#>  age           23.0 (3.9)  23.4 (4.0)  0.235  
#>  marijuana: No 97 (38.6%)  434 (37.3%) 1      
#>  illicit: No   205 (81.7%) 901 (77.4%) 0.623  
#>  rehab: No     121 (48.2%) 547 (47%)   0.729  
#> ───────────────────────────────────────────────
```

It can also do a total column with the stratified columns (new with
`v 1.9.0`) with the `total = TRUE` argument.

``` r
nhanes_2010 %>%
  group_by(asthma) %>%
  table1(age, marijuana, illicit, rehab,
         output = "text2",
         na.rm = FALSE,
         test = TRUE,
         type = "condense",
         total = TRUE)
#> 
#> ────────────────────────────────────────────────────────────
#>                                   asthma 
#>                Total        Yes         No          P-Value
#>                n = 1417     n = 251     n = 1164           
#>  ------------- ------------ ----------- ----------- -------
#>  age           23.3 (4.0)   23.0 (3.9)  23.4 (4.0)  0.201  
#>  marijuana: No 532 (37.5%)  97 (38.6%)  434 (37.3%) 1      
#>  illicit: No   1107 (78.1%) 205 (81.7%) 901 (77.4%) 0.623  
#>  rehab: No     668 (47.1%)  121 (48.2%) 547 (47%)   0.729  
#> ────────────────────────────────────────────────────────────
```

It can also report the statistics in addition to the p-values.

``` r
nhanes_2010 %>%
  group_by(asthma) %>%
  table1(age, marijuana, illicit, rehab,
         output = "text2",
         na.rm = FALSE,
         test = TRUE,
         total = TRUE,
         type = "full")
#> 
#> ─────────────────────────────────────────────────────────────────────────
#>                                       asthma 
#>            Total        Yes         No          Test             P-Value
#>            n = 1417     n = 251     n = 1164                            
#>  --------- ------------ ----------- ----------- ---------------- -------
#>  age                                            T-Test: -1.28    0.201  
#>            23.3 (4.0)   23.0 (3.9)  23.4 (4.0)                          
#>  marijuana                                      Chi Square: 0    1      
#>     Yes    716 (50.5%)  131 (52.2%) 584 (50.2%)                         
#>     No     532 (37.5%)  97 (38.6%)  434 (37.3%)                         
#>     NA     169 (11.9%)  23 (9.2%)   146 (12.5%)                         
#>  illicit                                        Chi Square: 0.24 0.623  
#>     Yes    141 (10%)    23 (9.2%)   117 (10.1%)                         
#>     No     1107 (78.1%) 205 (81.7%) 901 (77.4%)                         
#>     NA     169 (11.9%)  23 (9.2%)   146 (12.5%)                         
#>  rehab                                          Chi Square: 0.12 0.729  
#>     Yes    48 (3.4%)    10 (4%)     37 (3.2%)                           
#>     No     668 (47.1%)  121 (48.2%) 547 (47%)                           
#>     NA     701 (49.5%)  120 (47.8%) 580 (49.8%)                         
#> ─────────────────────────────────────────────────────────────────────────
```

`table1()` can be outputted directly to other formats. All
`knitr::kable()` options are available for this and there is an extra
option `"latex2"` which provides a publication ready table in Latex
documents.

The `tableC()` function gives a well-formatted correlation table.

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

The `tableF()` function gives a table of frequencies.

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
  mutate_rowmeans("avg_active", mod_active:vig_active) %>% 
  select(id, avg_active) %>% 
  head()
#>      id avg_active
#> 1 73568         NA
#> 2 73578        180
#> 3 73582         NA
#> 4 73585         45
#> 5 73592         NA
#> 6 73594         NA
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
