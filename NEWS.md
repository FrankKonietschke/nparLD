# nparLD 2.3.1

* Fixed the ordering of output rows and cell-effect plots for numeric factor
  levels. Output tables and plots now preserve the canonical factor-level order
  instead of applying lexicographic ordering to character representations of
  factor levels.

# nparLD 2.3.0

## Major redesign

* Added a general formula interface for crossed factorial longitudinal designs.
* Added inference for hypotheses in marginal distribution functions (`hypothesis = "H0F"`) and unweighted relative marginal effects (`hypothesis = "H0p"`).
* Added support for missing observations.
* Added support for dependent replicate measurements via the `replicate` argument.
* Added subject-level and observation-level cell weighting for relative marginal effects.
* Added multiple contrast procedures and simultaneous confidence intervals.
* Added rank- and pseudo-rank-based inference through a unified interface.
* Added the argument `covariance`, which optionally includes the estimated covariance matrix in the output.
* Added updated documentation, examples, README, vignette, and example datasets.
* Added the `brdu` data set for illustrating dependent replicate measurements.
* Added term-specific plots of factor-information estimates and confidence intervals via `plot(fit, term = ...)`.

## Interface changes

* Factor ordering is intended to follow ordinary R factor levels before calling `nparLD()`.
* Earlier design-specific ordering arguments are no longer part of the main interface.
* Classical designs such as `LD-F1`, `F1-LD-F1`, `LD-F2`, `F1-LD-F2`, and `F2-LD-F1` are now specified through the formula interface.

