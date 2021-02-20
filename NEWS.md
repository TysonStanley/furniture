# Version 1.9.10

- Fix bug related to updated `magrittr` package.
- Upgraded to GitHub actions for continuous integration.

# Version 1.9.8

- Very minor adjustment for `as.data.frame.table1()` to longer use `default.stringsAsFactors()` for r version >= 4.1.0

# Version 1.9.7

- Removed `crayon`, `rstudioapi`, and `tibble` as dependencies (they were part of the attachment message that has been discontinued)
- All calls to `data.frame()` or `as.data.frame()` have `stringsAsFactors = TRUE` for backward compatibility when R changes that default

# Version 1.9.5

- Removed `tidyverse` as a dependency (changed to `dplyr` since many other package in the `tidyverse` were not needed)
- Fixed printing bugs with `output = "latex2"`

# Version 1.9.4

Cleaned up the suggests and imports (removed `tidyverse` and a few others that were not necessary for the package to function). The message printed upon attachment is no longer displayed.

# Version 1.9.3

Added `rowmeans.n()` and `rowsums.n()` for row means and sums allowing `n` missing values across the row.


# Version 1.9.2

`table1()` now can do the non-parametric Kruskal-Wallis Rank Sum test (when `param = FALSE`). 

Also fixed a small issue with `mutate_rowmeans()` and `mutate_rowsums()`.

# Version 1.9.0

Added the option to have a total column in `table1()` even when using a grouping variable (`total = TRUE` will activate this option). The resulting tests (if `test = TRUE`) will give results of just the bivariate comparisons (grouping variable with the specified variable).

# Version 1.8.8

Small bug where a warning message was printed too often was fixed.

# Version 1.8.7

A minor update to fix an error that occurred when only a single variable was being summarized. Some code formatting under the hood was adjusted and some minor adjustments to the documentations were also made. Also, a warning is produced if any of the variables have no variability since this causes problems with tests and other formatting. Finally, a message is printed for `test = TRUE` when the variances are assumed to be unequal (based on a test). This is communicated in the documentation.

# Version 1.8.0

* When piping with `table1()`, it no longer returns the data frame. This feature caused too many unexpected issues. Instead, it returns the table.
* The class of `table1` is now no longer tied to the `data.frame`, for printing purposes. It is still possible to coerce to a `data.frame` using `as.data.frame()`.
* Using the `var_names` argument is now deprecated. In its place, it is recommended to name the variable in place (e.g., `"General Health" = gen_health` will produce the label `General Health` in the table instead of the variable name of `gen_health`).
* Fixed issue of having an underscore in a variable name in `output = "latex2"`.
* Added new attaching message more like the tidyverse to provide information about function conflicts.
* Added `mutate_rowmeans()` and `mutate_rowsums()` to more easily get the rowmeans/rowsums of a larger list of variables using the syntax that can be used within `dplyr::select()` (e.g., `var1:var30` for selecting `var1` through `var30` in the data frame).

# Version 1.7.9

Mainly bug fixes, including the removal of any empty rows. This allows for more accurate counts for the n's in really messy data. We also now re-export the pipe from `magrittr`.

# Version 1.7.6

Updates to the `"latex2"` output option for `table1()` and `tableC()` where it now produces a character vector of the output and then uses a print method to produce the Latex ready code. This means you can access the object and make changes to the table before having it print. It also outputs `\emph{Missing}` when `na.rm=FALSE`. Thanks to [Joshua Pritikin](https://github.com/jpritikin) for the ideas and the pull requests leading to these improvements.

Other minor bug fixes.

`%xt%` is now deprecated. `tableF()` and `tableX()` effectively do what `%xt%` was designed to do, but with better and more thorough output.


# Version 1.7.3

Minor changes and two new functions:

1. `rowmeans()` provides a tidyverse friendly `rowMeans()`
2. `rowsums()` provides a tidyverse friendly `rowSums()`

# Version 1.7.2

Several new features were added:

1. `table1()` can take a `grouped_df` from the tidyverse's `group_by()`. This also means `table1()` can take on any number of stratifying variables in a clean way.
2. `table1()` also has a new 'latex2' output option. This is not dependent on `knitr::kable()` and is better suited for this situation. Should provide better latex tables than before.
3. `tableF()` is a new function that provides frequency tables (potentially by a grouping variable).
4. General formatting was improved.
5. Other internal improvements that should help with error-catching, syntax simplicity, and flexibility for the user.


# Version 1.6.0

A new function was added: `tableC()`, which, like `table1()` provides a nicely formatted table. However, `tableC()` provides correlations instead of the descriptive statistics that  `table1()` focuses on.

We are also deprecating the `var_names` argument in `table1()` since it is now possible to name the variable from within the function itself without relying on a separate argument.

Other small bug fixes.


# Version 1.5.4

There are three notable changes in this update:

1. Small bug fix when using piping and ask for one of the kable output options. This has now been fixed. Additionally, better error catching and improved function tests.
2. The `long()` function has been added to the package. It is a wrapper of the `stats::reshape()` function with a direction set to "long", but has added benefits, including a bit cleaner syntax, the ability to handle unbalanced data, and works well with the tidyverse.
3. The `wide()` function has been added as well. It, like `long()`, is a wrapper of `reshape()` but with a set direction of "wide" and has added benefits as well.


# Version 1.5.0

`table1()` now has the ability to apply any function to numeric variables that the user supplies (all functions that work with `tapply()` should work with `table1()`. Other than that, the changes are mainly internal changes in `table1()` that have bearing on a few arguments.

1. If no variables are named, then all variables are summarized in the `data.frame` (the `all` argument is no longer used).
2. Piping is automatically detected so `piping` is not longer used as an argument.
3. `rounding` is no longer used given a user can define their own function with their own rounding limits.
4. `test_type` is no longer an argument. The functionality of the `"or"` option was just not being used and was too cumbersome.
5. `format_output`, `condense` and `simple` are combined into one `type` argument (defaults are still the same).
6. `output_type` is now just `output`.

Finally, `tableM()` was removed from the package. It is easily adopted by `table1()` with small modifications to the `splitby` variable. I apologize for any inconvenience these changes cause. However, I believe it is best in the long run and will make using and upgrading the package much easier.

As an aside, most of these changes are due to a manuscript in review about the package. Several beneficial suggestions were made and so we made those changes at the cost of a small headache at first.

Thanks!


# Version 1.4.1

Three big changes:

1. `simple_table1` is no longer with us. The functionality has been integrated into `table1`.
2. `table1` has a "condensed" version that leaves less white space and is more in line with some academic journals.
3. The "simple" option has been added to `table1` which allows only the percentages (instead of counts and percentages) for categorical variables.
4. `tableM` is a new function that can analyze a missing data by putting a variable with missingness in the "missing_var" argument.

Enjoy!


# Version 1.3.0

Three notable changes:

1. A new function--`tableM`--has been added to the package. It produces a table much like `table1` but analyzes a variable with missingness.
2. `table1` (and its related functions such as `simple_table1` and `tableM`) now has better formatting and has an additional option `output_type = "text2"` which provides lines to separate the header from the rest of the table.
3. `table1` can give percentages based on the row using `row_wise = TRUE`. The default is giving percentages by column.

Also, a bug fix:

1. `table1` was rounding the percentages at the ten's place instead of the tenth's place. It now is fixed.

Thanks!

# Version 1.2.3

A bug that rounded the percentages to the nearest tenth place was found and fixed for both `table1` and `simple_table1`.


# Version 1.2.2

An error was found when using `table1` with a splitby factor with more than 2 levels. This has been fixed in version 1.2.2.

There were also two notable inclusions:

1. In `table1` you can now format the numbers in the output with a comma for large numbers (e.g., 20,000 vs. 20000).
2. In `table1` you can also calculate percentages of factor variables across rows instead of within groups.
3. A version of `table1` known as `simple_table1` that reports the percentages of factors instead of counts and percentages.
4. A new vignette all about `table1` is included.

None of these changes will break programs already in place using this package.

Thanks.


# Version 1.2.0

We are excited to announce that we are updating our `furniture` package from 1.0.1 to 1.2.0. We included a new operator (`%xt%`) for simple cross tabulations that includes a chi-square test and some data from NHANES 2005-2010. Although these additions are nice, most of the updates were to `table1`. These included:

1. Bug fixes (the function got confused if there was an object in the global environment that had the same name as a variable in the data frame you were analyzing)
2. Fewer dependencies (this will allow the package to be more stable across platforms with different package versions loaded without having to worry about packrat)
3. Additional functionality in the splitby argument (the splitby argument can now have a formula [e.g. ~var] or can be a string [e.g. "var"])
4. Additional functionality in the output_type argument (can now do any type of output that `knitr::kable` can do plus the regular "text" option)

None of these changes will break programs already in place using this package. Instead, there are additional uses of the functions as shown in points 3 and 4.

Thanks.
