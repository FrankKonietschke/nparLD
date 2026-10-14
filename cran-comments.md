## R CMD check results

0 errors | 0 warnings | 0 notes

## Additional checks

* Local R CMD check --as-cran: OK
* win-builder R-devel: OK

## Comments

This is an update to version 2.3.3.

The update revises the `print()` and `summary()` methods for fitted `nparLD`
objects. `print()` now provides a concise model overview with the ANOVA-type
statistic, whereas `summary()` provides detailed effect estimates and
inferential results.

Documentation, examples, tests, and the package vignette were updated
accordingly.