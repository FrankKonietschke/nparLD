nparLD
================

# nparLD

`nparLD` provides nonparametric methods for the analysis of longitudinal
and repeated-measures data in factorial experiments. The package
implements procedures for hypotheses in marginal distribution functions
and in unweighted relative marginal effects. It supports crossed
factorial designs, missing observations, dependent replicate
measurements, rank- and pseudo-rank-based inference, Wald-type and
ANOVA-type statistics, multiple contrast procedures, and simultaneous
confidence intervals.

## Installation

You can install the development version locally with:

``` r
devtools::install()
```

## Basic example

``` r
library(nparLD)

set.seed(123)

T <- 4
n <- c(10, 10, 10)
N <- sum(T * n)

y <- rnorm(N)
times <- c(rep(1:T, n[1]), rep(1:T, n[2]), rep(1:T, n[3]))
grp <- c(rep(1, n[1] * T), rep(2, n[2] * T), rep(3, n[3] * T))
subjects <- c(
  sort(rep(1:n[1], T)),
  sort(rep((n[1] + 1):(n[1] + n[2]), T)),
  sort(rep((n[1] + n[2] + 1):(n[1] + n[2] + n[3]), T))
)

data <- data.frame(times, grp, subjects, y)

fit <- nparLD(
  y ~ grp * times,
  data = data,
  subject = "subjects",
  hypothesis = "H0p",
  contrast = list("grp:times")
)

fit
plot(fit)
plot(fit$MCTP)
```

## Weighted and unweighted relative effects

The package distinguishes between weighted and unweighted relative
marginal effects.

Weighted effects are based on the sample-size weighted reference
distribution. They describe relative positions with respect to the
empirical distribution obtained by pooling the marginal samples
according to their observed sample sizes. Weighted effects are useful as
descriptive summaries and correspond to ordinary rank-based estimators.

Unweighted effects are based on an equally weighted reference
distribution over the factorial cells. Each cell contributes equally to
the reference distribution, independently of its sample size. These
effects are the inferential target for hypotheses in relative marginal
effects, selected by `hypothesis = "H0p"`, and are estimated using
pseudo-ranks.

Consequently, tests of hypotheses in relative marginal effects are
performed for unweighted effects. Weighted effects are reported
descriptively, but they are not used as the target of `H0p` tests
because their interpretation depends on the sample-size allocation
across cells.

\`\`\`\`markdown The following example illustrates how the two effect
concepts are requested.

``` r
data(panic2)

fit_weighted <- nparLD(
  resp ~ group * time,
  data = panic2,
  subject = "subject",
  effect = "weighted",
  hypothesis = "H0F"
)

fit_unweighted <- nparLD(
  resp ~ group * time,
  data = panic2,
  subject = "subject",
  effect = "unweighted",
  hypothesis = "H0F"
)

fit_weighted
fit_unweighted
```

The first analysis reports weighted effects as descriptive summaries
together with tests in marginal distribution functions. The second
analysis targets unweighted relative marginal effects and therefore
provides the appropriate effect scale for `H0p`.

## Hypothesis types

Use `hypothesis = "H0F"` to test hypotheses in marginal distribution
functions. These tests compare marginal distribution functions either
using classical ranks with the argument `effect="weighted"` or using
pseudo-ranks using the argument `effect = "unweighted"`.

Use `hypothesis = "H0p"` to test hypotheses in unweighted relative
marginal effects. These effects describe the relative position of each
marginal distribution with respect to a common unweighted reference
distribution and are useful for interpretation, contrasts, simultaneous
confidence intervals, and plots.

## Dependent replicates

Dependent replicate measurements can be specified with the `replicate`
argument.

``` r
datensatz2 <- datensatz[rep(seq_len(nrow(datensatz)), each = 3), ]
datensatz2$rep <- rep(1:3, times = nrow(datensatz))
datensatz2$y <- rnorm(nrow(datensatz2))

fit_rep <- nparLD(
  y ~ grp * times,
  data = datensatz2,
  subject = "subjekte",
  replicate = "rep",
  hypothesis = "H0p",
  cell.weights = "subjects"
)

fit_rep
```

For relative marginal effects, `cell.weights = "subjects"` targets a
typical subject-condition cell, while `cell.weights = "observations"`
targets a typical replicate observation.

## Classical longitudinal designs

Earlier versions of `nparLD` used design-specific functions such as
`ld.f1()`, `f1.ld.f1()`, or `f2.ld.f1()`. In the redesigned package,
these designs are specified through one general formula interface.

Typical examples are:

| Classical design | Formula in the redesigned interface | Example |
|----|----|----|
| `LD-F1` | `resp ~ time` | one repeated-measures factor |
| `F1-LD-F1` | `resp ~ group * time` | one whole-plot and one repeated-measures factor |
| `LD-F2` | `resp ~ time1 * time2` | two repeated-measures factors |
| `F1-LD-F2` | `resp ~ group * time1 * time2` | one whole-plot and two repeated-measures factors |
| `F2-LD-F1` | `resp ~ group1 * group2 * time` | two whole-plot and one repeated-measures factor |

For example, the shoulder data correspond to an `F2-LD-F1` design:

``` r
data(shoulder)

fit <- nparLD(
  resp ~ group1 * group2 * time,
  data = shoulder,
  subject = "subject",
  hypothesis = "H0p",
  contrast = list("group1:time")
)

fit
plot(fit)
plot(fit$MCTP)
```
