## Test environments
* local Mac OS X, R 3.3.1
* Ubuntu 12.04 (through Travis-CI)
* Win-Builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs (none in Mac/Ubuntu and 2 in Windows) both dealing with the run-time of tp2frames function:

* checking examples ... running examples for arch 'i386' ... [34s] NOTE
    - Examples with CPU or elapsed time > 10s: user 25.96; system 0.07; elapsed 26.02
* checking examples ... running examples for arch 'x64' ... [46s] NOTE
    - Examples with CPU or elapsed time > 10s: user 34.95; system 0.03; elapsed 34.99

Given this is a bootstrapping function that deals with two models simulateneously, it is expected to run more slowly.