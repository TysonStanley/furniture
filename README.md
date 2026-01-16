
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version/furniture)](https://cran.r-project.org/package=furniture)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/furniture)
[![R-CMD-check](https://github.com/TysonStanley/furniture/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/TysonStanley/furniture/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# furniture: `v1.10.0` <img src="man/figures/furniture_hex_v2_full.png" align="right" width="40%" height="40%" />

The furniture R package contains functions to help with data
cleaning/tidying (e.g., `washer()`, `rowmeans()`, `rowsums()`),
exploratory data analysis and reporting (e.g., `table1()`, `tableC()`,
`tableF()`). It currently contains eight main functions:

1.  `table1()` : gives a well-formatted table for academic publication
    of descriptive statistics. Very useful for quick analyses as well.
    Notably, `table1()` now works with `dplyr::group_by()`.
2.  `tableC()` : gives a well-formatted table of correlations.
3.  `tableF()` : provides a thorough frequency table for quick checks of
    the levels of a variable.
4.  `washer()` : changes several values in a variable (very useful for
    changing place holder values to missing).
5.  `long()` : is a wrapper of `stats::reshape()`, takes the data from
    wide to long format (long is often the tidy version of the data),
    works well with the tidyverse, and can handle unbalanced multilevel
    data.
6.  `wide()` : also a wrapper of `stats::reshape()`, takes the data from
    long to wide, and like `long()`, works well with the tidyverse and
    can handle unbalanced multilevel data.
7.  `rowmeans()` and `rowmeans.n()` : tidyverse friendly versions of
    `rowMeans()`, where the `rowmeans.n()` function allows `n` number of
    missing
8.  `rowsums()` and `rowsums.n()` : tidyverse friendly versions of
    `rowSums()`, where the `rowsums.n()` function allows `n` number of
    missing

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
remotes::install_github("tysonstanley/furniture")
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

A new function `table1_gt()` integrates the fantastic `gt` package to
make beautiful HTML tables for table1.

``` r
nhanes_2010 %>%
  group_by(asthma) %>%
  table1(age, marijuana, illicit, rehab, na.rm = FALSE) %>%
  table1_gt() %>%
  print()
#> Using dplyr::group_by() groups: asthma
```

<div id="mdnriqyfnw"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>\#mdnriqyfnw table { font-family: system-ui, ‘Segoe UI’, Roboto,
Helvetica, Arial, sans-serif, ‘Apple Color Emoji’, ‘Segoe UI Emoji’,
‘Segoe UI Symbol’, ‘Noto Color Emoji’; -webkit-font-smoothing:
antialiased; -moz-osx-font-smoothing: grayscale; }

\#mdnriqyfnw thead, \#mdnriqyfnw tbody, \#mdnriqyfnw tfoot, \#mdnriqyfnw
tr, \#mdnriqyfnw td, \#mdnriqyfnw th { border-style: none; }

\#mdnriqyfnw p { margin: 0; padding: 0; }

\#mdnriqyfnw .gt_table { display: table; border-collapse: collapse;
line-height: normal; margin-left: auto; margin-right: auto; color:
\#333333; font-size: 16px; font-weight: normal; font-style: normal;
background-color: \#FFFFFF; width: auto; border-top-style: solid;
border-top-width: 2px; border-top-color: \#A8A8A8; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3;
border-bottom-style: solid; border-bottom-width: 2px;
border-bottom-color: \#A8A8A8; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; }

\#mdnriqyfnw .gt_caption { padding-top: 4px; padding-bottom: 4px; }

\#mdnriqyfnw .gt_title { color: \#333333; font-size: 125%; font-weight:
initial; padding-top: 4px; padding-bottom: 4px; padding-left: 5px;
padding-right: 5px; border-bottom-color: \#FFFFFF; border-bottom-width:
0; }

\#mdnriqyfnw .gt_subtitle { color: \#333333; font-size: 85%;
font-weight: initial; padding-top: 3px; padding-bottom: 5px;
padding-left: 5px; padding-right: 5px; border-top-color: \#FFFFFF;
border-top-width: 0; }

\#mdnriqyfnw .gt_heading { background-color: \#FFFFFF; text-align:
center; border-bottom-color: \#FFFFFF; border-left-style: none;
border-left-width: 1px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 1px; border-right-color: \#D3D3D3; }

\#mdnriqyfnw .gt_bottom_border { border-bottom-style: solid;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3; }

\#mdnriqyfnw .gt_col_headings { border-top-style: solid;
border-top-width: 2px; border-top-color: \#D3D3D3; border-bottom-style:
solid; border-bottom-width: 2px; border-bottom-color: \#D3D3D3;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; }

\#mdnriqyfnw .gt_col_heading { color: \#333333; background-color:
\#FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: bottom; padding-top: 5px;
padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x:
hidden; }

\#mdnriqyfnw .gt_column_spanner_outer { color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: normal;
text-transform: inherit; padding-top: 0; padding-bottom: 0;
padding-left: 4px; padding-right: 4px; }

\#mdnriqyfnw .gt_column_spanner_outer:first-child { padding-left: 0; }

\#mdnriqyfnw .gt_column_spanner_outer:last-child { padding-right: 0; }

\#mdnriqyfnw .gt_column_spanner { border-bottom-style: solid;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3; vertical-align:
bottom; padding-top: 5px; padding-bottom: 5px; overflow-x: hidden;
display: inline-block; width: 100%; }

\#mdnriqyfnw .gt_spanner_row { border-bottom-style: hidden; }

\#mdnriqyfnw .gt_group_heading { padding-top: 8px; padding-bottom: 8px;
padding-left: 5px; padding-right: 5px; color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: initial;
text-transform: inherit; border-top-style: solid; border-top-width: 2px;
border-top-color: \#D3D3D3; border-bottom-style: solid;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: middle; text-align: left;
}

\#mdnriqyfnw .gt_empty_group_heading { padding: 0.5px; color: \#333333;
background-color: \#FFFFFF; font-size: 100%; font-weight: initial;
border-top-style: solid; border-top-width: 2px; border-top-color:
\#D3D3D3; border-bottom-style: solid; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; vertical-align: middle; }

\#mdnriqyfnw .gt_from_md \> :first-child { margin-top: 0; }

\#mdnriqyfnw .gt_from_md \> :last-child { margin-bottom: 0; }

\#mdnriqyfnw .gt_row { padding-top: 8px; padding-bottom: 8px;
padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style:
solid; border-top-width: 1px; border-top-color: \#D3D3D3;
border-left-style: none; border-left-width: 1px; border-left-color:
\#D3D3D3; border-right-style: none; border-right-width: 1px;
border-right-color: \#D3D3D3; vertical-align: middle; overflow-x:
hidden; }

\#mdnriqyfnw .gt_stub { color: \#333333; background-color: \#FFFFFF;
font-size: 100%; font-weight: initial; text-transform: inherit;
border-right-style: solid; border-right-width: 2px; border-right-color:
\#D3D3D3; padding-left: 5px; padding-right: 5px; }

\#mdnriqyfnw .gt_stub_row_group { color: \#333333; background-color:
\#FFFFFF; font-size: 100%; font-weight: initial; text-transform:
inherit; border-right-style: solid; border-right-width: 2px;
border-right-color: \#D3D3D3; padding-left: 5px; padding-right: 5px;
vertical-align: top; }

\#mdnriqyfnw .gt_row_group_first td { border-top-width: 2px; }

\#mdnriqyfnw .gt_row_group_first th { border-top-width: 2px; }

\#mdnriqyfnw .gt_summary_row { color: \#333333; background-color:
\#FFFFFF; text-transform: inherit; padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; }

\#mdnriqyfnw .gt_first_summary_row { border-top-style: solid;
border-top-color: \#D3D3D3; }

\#mdnriqyfnw .gt_first_summary_row.thick { border-top-width: 2px; }

\#mdnriqyfnw .gt_last_summary_row { padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; border-bottom-style: solid;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3; }

\#mdnriqyfnw .gt_grand_summary_row { color: \#333333; background-color:
\#FFFFFF; text-transform: inherit; padding-top: 8px; padding-bottom:
8px; padding-left: 5px; padding-right: 5px; }

\#mdnriqyfnw .gt_first_grand_summary_row { padding-top: 8px;
padding-bottom: 8px; padding-left: 5px; padding-right: 5px;
border-top-style: double; border-top-width: 6px; border-top-color:
\#D3D3D3; }

\#mdnriqyfnw .gt_last_grand_summary_row_top { padding-top: 8px;
padding-bottom: 8px; padding-left: 5px; padding-right: 5px;
border-bottom-style: double; border-bottom-width: 6px;
border-bottom-color: \#D3D3D3; }

\#mdnriqyfnw .gt_striped { background-color: rgba(128, 128, 128, 0.05);
}

\#mdnriqyfnw .gt_table_body { border-top-style: solid; border-top-width:
2px; border-top-color: \#D3D3D3; border-bottom-style: solid;
border-bottom-width: 2px; border-bottom-color: \#D3D3D3; }

\#mdnriqyfnw .gt_footnotes { color: \#333333; background-color:
\#FFFFFF; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3; }

\#mdnriqyfnw .gt_footnote { margin: 0px; font-size: 90%; padding-top:
4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; }

\#mdnriqyfnw .gt_sourcenotes { color: \#333333; background-color:
\#FFFFFF; border-bottom-style: none; border-bottom-width: 2px;
border-bottom-color: \#D3D3D3; border-left-style: none;
border-left-width: 2px; border-left-color: \#D3D3D3; border-right-style:
none; border-right-width: 2px; border-right-color: \#D3D3D3; }

\#mdnriqyfnw .gt_sourcenote { font-size: 90%; padding-top: 4px;
padding-bottom: 4px; padding-left: 5px; padding-right: 5px; }

\#mdnriqyfnw .gt_left { text-align: left; }

\#mdnriqyfnw .gt_center { text-align: center; }

\#mdnriqyfnw .gt_right { text-align: right; font-variant-numeric:
tabular-nums; }

\#mdnriqyfnw .gt_font_normal { font-weight: normal; }

\#mdnriqyfnw .gt_font_bold { font-weight: bold; }

\#mdnriqyfnw .gt_font_italic { font-style: italic; }

\#mdnriqyfnw .gt_super { font-size: 65%; }

\#mdnriqyfnw .gt_footnote_marks { font-size: 75%; vertical-align: 0.4em;
position: initial; }

\#mdnriqyfnw .gt_asterisk { font-size: 100%; vertical-align: 0; }

\#mdnriqyfnw .gt_indent_1 { text-indent: 5px; }

\#mdnriqyfnw .gt_indent_2 { text-indent: 10px; }

\#mdnriqyfnw .gt_indent_3 { text-indent: 15px; }

\#mdnriqyfnw .gt_indent_4 { text-indent: 20px; }

\#mdnriqyfnw .gt_indent_5 { text-indent: 25px; }

\#mdnriqyfnw .katex-display { display: inline-flex !important;
margin-bottom: 0.75em !important; }

\#mdnriqyfnw div.Reactable \> div.rt-table \> div.rt-thead \>
div.rt-tr.rt-tr-group-header \> div.rt-th-group:after { height: 0px
!important; } </style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">

<thead>

<tr class="gt_col_headings">

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Characteristic">

Characteristic
</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Yes,-n-=-251">

Yes, n = 251
</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="No,-n-=-1164">

No, n = 1164
</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">age</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md"></span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md">23.0 (3.9)</span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md">23.4 (4.0)</span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">marijuana</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">     Yes</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md">131 (52.2%)</span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md">584 (50.2%)</span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">     No</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md">97 (38.6%)</span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md">434 (37.3%)</span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">     NA</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md">23 (9.2%)</span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md">146 (12.5%)</span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">illicit</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">     Yes</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md">23 (9.2%)</span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md">117 (10.1%)</span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">     No</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md">205 (81.7%)</span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md">901 (77.4%)</span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">     NA</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md">23 (9.2%)</span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md">146 (12.5%)</span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">rehab</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md"></span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">     Yes</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md">10 (4%)</span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md">37 (3.2%)</span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">     No</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md">121 (48.2%)</span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md">547 (47%)</span>
</td>

</tr>

<tr>

<td headers="Characteristic" class="gt_row gt_left">

<span class="gt_from_md">     NA</span>
</td>

<td headers="Yes, n = 251" class="gt_row gt_right">

<span class="gt_from_md">120 (47.8%)</span>
</td>

<td headers="No, n = 1164" class="gt_row gt_right">

<span class="gt_from_md">580 (49.8%)</span>
</td>

</tr>

</tbody>

</table>

</div>

Alternatively, `table1_flextable()` integrates the `flextable` package
for flexible and customizable table output (especially useful for Word
documents).

``` r
nhanes_2010 %>%
  group_by(asthma) %>%
  table1(age, marijuana, illicit, rehab, na.rm = FALSE) %>%
  table1_flextable(spanner = "Asthma")
```

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
  mutate(sum_active = rowsums(vig_active, mod_active, na.rm=TRUE)) %>% 
  head()
#>   vig_active mod_active avg_active sum_active
#> 1         30         NA         30         30
#> 2        180        180        180        360
#> 3         NA         NA        NaN          0
#> 4         20         70         45         90
#> 5        120         NA        120        120
#> 6         NA         NA        NaN          0
```

The `rowmeans.n()` and `rowsums.n()` allow `n` missing values while
still calculating the mean or sum.

``` r
df <- data.frame(
  x = c(NA, 1:5),
  y = c(1:5, NA)
)

df %>%
  mutate(avg = rowmeans.n(x, y, n = 1))
#>    x  y avg
#> 1 NA  1 1.0
#> 2  1  2 1.5
#> 3  2  3 2.5
#> 4  3  4 3.5
#> 5  4  5 4.5
#> 6  5 NA 5.0
```

## Notes

The package is most useful in conjunction with other tidy tools to get
data cleaned/tidied and start exploratory data analysis. I recommend
using packages such as `library(dplyr)`, `library(tidyr)`, and
`library(ggplot2)` with `library(furniture)` to accomplish this.

The original function–`table1()`–is simply built for both exploratory
descriptive analysis and communication of findings. See vignettes or
[tysonbarrett.com](https://tysonstanley.github.io/) for several examples
of its use. Also see our paper in the [R
Journal](https://journal.r-project.org/articles/RJ-2017-037/RJ-2017-037.pdf).
