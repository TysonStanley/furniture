# Version 1.5.0

`table1()` now has the ability to apply any function to numeric variables that the user supplies (all functions that work with `tapply()` should work with `table1()`. Other than that, the changes are mainly internal changes in `table1()` that have bearing on a few arguments.

1. If no variables are named, then all variables are summarized in the `data.frame` (the `all` argument is no longer used).
2. Piping is automatically detected so `piping` is not longer used as an argument.
3. `rounding` is no longer used given a user can define their own function with their own rounding limits.
4. `test_type` is no longer an argument. The functionality of the `"or"` option was just not being used and was too cumbersome.

I apologize for any inconvenience these changes cause. However, I believe it is best in the long run and will make using and upgrading the package much easier.

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
2. `table1` (and its related functons such as `simple_table1` and `tableM`) now has better formatting and has an additional option `output_type = "text2"` which provides lines to separate the header from the rest of the table.
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
