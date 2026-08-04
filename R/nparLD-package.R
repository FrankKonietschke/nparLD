
#' nparLD: Nonparametric Longitudinal Data Analysis
#'
#' The \pkg{nparLD} package provides nonparametric methods for the analysis of
#' longitudinal and repeated-measures data in factorial experiments. It is
#' designed for settings in which response distributions may be non-normal,
#' ordinal, skewed, heteroscedastic, or affected by ties. The procedures do not 
#' require distributional assumptions, and are applicable to a variety of data types 
#'(continuous, discrete, purely ordinal, and dichotomous). Such methods are also robust 
#'with respect to outliers and for small sample sizes.
#'
#' The main function is [nparLD()], which implements inference for hypotheses in
#' marginal distribution functions and in unweighted relative marginal effects.
#' The package supports crossed factorial designs with whole-plot and subplot
#' factors, missing observations, dependent replicate measurements, rank- and
#' pseudo-rank-based estimation, Wald-type and ANOVA-type statistics, multiple
#' contrast procedures, and simultaneous confidence intervals.
#'
#' @section Main function:
#' \itemize{
#'   \item [nparLD()] performs the nonparametric analysis.
#' }
#'
#' @section Hypotheses:
#' The argument \code{hypothesis = "H0F"} tests hypotheses in marginal
#' distribution functions. The argument \code{hypothesis = "H0p"} tests
#' hypotheses in unweighted relative marginal Mann-Whitney effects and thus addresses 
#' the nonparametric Behrens-Fisher problem in factorial longitudinal designs.
#'
#' @section Effects:
#' The argument \code{effect = "weighted"} estimates weighted relative marginal Mann-Whitney effects
#' using classical ranks (mid-ranks) of the observations. The argument \code{effect = "unweighted"} 
#' estimates unweighted relative marginal Mann-Whitney effects using pseudo-ranks of the data. The weighted relative marginal
#' effect depends on sample sizes and their allocations, whereas the unweighted relative marginal effect does not.
#'
#' @section Contrast:
#' The argument \code{contrast = list()} estimates and tests contrasts on the given factor levels or their interaction effects
#' using multiple contrast tests. If the null hypothesis H0p is tested, then simultaneous confidence intervals are computed.
#'
#'
#' @section Replicates:
#' Dependent replicates can be specified by the \code{replicate} argument.
#' For relative marginal effects, the \code{cell.weights} argument determines
#' whether subject-condition cells or individual replicate observations define
#' the target of estimation.
#'
#' @keywords internal
#' @importFrom graphics abline axis points segments
#' @importFrom stats aggregate as.formula ave cov2cor model.frame na.pass pchisq pf pt qnorm qt quantile rbinom reshape sd setNames terms var
#' @importFrom rlang .data
"_PACKAGE"

