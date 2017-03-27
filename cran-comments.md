## Test environments
* local Mac OS X 10.11, R 3.3.2
* Ubuntu 12.04 (through Travis-CI), R 3.3.2, R 3.2.5, R Under development
* Win-Builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream Dependencies
Currently, there are no downstream dependencies relating to this package.

## Quick Resubmission
I apologize for the quick resubmission. There was a very minor code problem that stopped the kable output options when the user piped the data into `table1()`. It did not produce any errors, and therefore, was not originally caught. This has been fixed in the code. 

No other changes occurred.