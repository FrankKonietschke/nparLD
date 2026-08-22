## R CMD check results

0 errors | 0 warnings | 0 notes

## Additional checks

* Local R CMD check --as-cran: OK
* win-builder R-devel: OK
* R-hub/GitHub Actions: OK on linux, macOS, macOS arm64, Windows, and ubuntu-release
* Reverse dependencies checked: 2, OK: 2, BROKEN: 0

## Reverse dependencies

Checked 2 reverse dependencies:

* MANOVA.RM
* colleyRstats

No reverse dependency failures were found.

## Comments

This is a major update of nparLD. The package now provides a formula interface for crossed factorial 
longitudinal and repeated-measures designs that support for incomplete observations, dependent replicate measurements, hypotheses in marginal distribution functions and unweighted relative marginal effects, multiple contrast procedures, simultaneous confidence intervals, covariance output, factor-specific effect summaries, and plotting methods.
The previous CRAN NOTE about the missing Authors@R field has been addressed. The DESCRIPTION file now contains Authors@R. A modern inst/CITATION file using bibentry() has also been added.
