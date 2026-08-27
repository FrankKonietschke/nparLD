## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

* checking CRAN incoming feasibility ... NOTE
  Days since last update: 5

This is a small patch release submitted shortly after version 2.3.0 because it
fixes an output-ordering bug for numeric factor levels. No methodological
changes were made.

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

This is a small patch release submitted shortly after version 2.3.0 because it
fixes an output-ordering bug for numeric factor levels in output tables and
cell-effect plots. No methodological changes were made.