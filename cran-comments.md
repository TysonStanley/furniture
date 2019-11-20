## Test environments
* local OS X install, R 3.6.1
* ubuntu 12.04 (on travis-ci), R 3.6.1, R underdevelopment
* ubuntu 16.04 LTS, R-release, GCC (rhub)
* fedobra R-devel, clang, gfortran (rhub)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (rhub)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

This update removes `tidyverse` from suggests and vignettes 
(as it is not a package but a collection of packages).

## Downstream Dependencies
Currently, there is one downstream dependency for this package (`MarginalMediation`). 
This package has been checked with no errors, warnings, or notes resulting from the changes
to the `furniture` package.

