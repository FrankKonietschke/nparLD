## Test environments

* Local Windows 11, R 4.5.1, `R CMD check --as-cran`
* win-builder, R-devel
* win-builder, R-release

## R CMD check results

0 errors | 0 warnings | 1 note

The remaining NOTE in the local check was:

```text
checking for future file timestamps ... NOTE
unable to verify current time
```

This NOTE was specific to the local environment. The same package passed
both win-builder R-devel and R-release with `Status: OK`.

## Changes

This update synchronizes the CRAN package with the version used in the
revised manuscript.

It includes updates to the package interface and documentation, revised
print and summary methods, corrected standard-error scaling for reported
relative effects, updated covariance documentation, revised multiple
contrast procedures, permutation inference, additional accessor and plot
methods, and updates to the Shiny application.

All examples, tests, vignettes, vignette rebuilding, and package
documentation checks completed successfully.