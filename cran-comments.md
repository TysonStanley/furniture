## Test environments
* local Mac OS X 10.11, R 3.3.1
* Ubuntu 12.04 (through Travis-CI), R 3.3.1, R 3.2.5, R Under development
* Win-Builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream Dependencies
Currently, there are no downstream dependencies relating to this package.

------

## Reason for Quick Resubmission
* A fatal error in version 1.2.0 was found and is fixed in version 1.2.2.
* The testing file now accounts for 95% of the package and should catch errors like this in the future.

Thank you!