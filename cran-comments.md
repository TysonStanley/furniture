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

I apologize for the quick resubmission. A fatal error with the main function (table1) was found. 

* The fatal error in version 1.2.0 is fixed in version 1.2.2.
* The testing files now account for 95% of the package and should catch errors like this in the future.

Thank you!