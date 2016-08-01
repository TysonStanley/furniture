# furniture

The furniture R package contains functions to help create summary tables for academic publication as well as a few data cleaning, modeling, and model summary techniques (e.g., obtaining average marginal effects from GLM models). It currently contains seven main functions:

1. `table1()` -- gives a well-formatted table for academic publication of descriptive statistics. Very useful for quick analyses as well.
2. `tableX()` -- gives model summaries of several "lm" and "glm" models simultaneously
3. `frames()` -- produces average marginal effects of glm models
4. `tp()` -- performs a two-part model for zero inflated count data
5. `tp2frames()` -- takes a tp object and gives the combined average marginal effects
6. `mirror()` -- reverse codes a variable
7. `washer()` -- changes several values in a variable (very useful for changing place holder values to missing)

Overall, the package should be useful for social scientists working on quantitative research.

## Table 1

`table1()` is designed to produce a table of descriptive statistics often found in the first table of an academic article in the health, behavioral and social sciences. It has options of producing counts and means by a stratifying variable, testing for bivatiate effects of the variable by the stratifying variable as well as some cosmetic options for producing the table.

More to come...



