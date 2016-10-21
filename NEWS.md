# Version 1.2.0

We are excited to announce that we are updating our `furniture` package from 1.0.1 to 1.2.0. We included a new operator (`%xt%`) for simple cross tabulations that includes a chi-square test and some data from NHANES 2005-2010. Although these additions are nice, most of the updates were to `table1`. These included:

1. Bug fixes (the function got confused if there was an object in the global environment that had the same name as a variable in the data frame you were analyzing)
2. Fewer dependencies (this will allow the package to be more stable across platforms with different package versions loaded without having to worry about packrat)
3. Additional functionality in the splitby argument (the splitby argument can now have a formula [e.g. ~var] or can be a string [e.g. "var"])
4. Additional functionality in the output_type argument (can now do any type of output that `knitr::kable` can do plus the regular "text" option)

None of these changes will break programs already in place using this package. Instead, there are additional uses of the functions as shown in points 3 and 4.

Thanks.
