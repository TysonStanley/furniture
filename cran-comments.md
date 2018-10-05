## Test environments
* local Mac OS X 10.12, R 3.5.1
* Ubuntu 12.04 (through Travis-CI), R 3.5.0, R 3.3.3, R Under development
* Win-Builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream Dependencies
Currently, there is one downstream dependency for this package (`MarginalMediation`). 
This package has been checked with no errors, warnings, or notes resulting from the changes
to the `furniture` package.

## Resubmission
An error that impacted a few edge cases was found and fixed. A unit test was included to catch these
edge cases.