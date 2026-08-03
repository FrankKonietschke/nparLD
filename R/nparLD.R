#' Nonparametric Analysis of Longitudinal Data
#'
#' Performs nonparametric inference for longitudinal or repeated-measures data 
#' from factorial experiments. The function supports hypotheses about marginal 
#' distribution functions and unweighted relative marginal Mann-Whitney effects. 
#' It allows arbitrary crossed whole-plot and subplot factors, incomplete observations, 
#' and dependent replicate measurements.
#'
#' @param formula A model formula of the form \code{response ~ factors}. Factors 
#' on the right-hand side may contain crossed whole-plot and subplot factors 
#' and their interactions. 
#' @param data A data frame containing the response, subject variable, design 
#' factors, and optionally a replicate variable. 
#' @param subject Character string specifying the subject identifier. 
#' @param replicate Optional character string specifying the replicate identifier. 
#' Replicates are assumed to be (possibly) dependent within subject-condition cells. 
#' @param hypothesis Character string specifying the hypothesis type. Use 
#' \code{"H0F"} for hypotheses in marginal distribution functions and 
#' \code{"H0p"} for hypotheses in unweighted relative marginal effects. 
#' @param effect Character string specifying whether weighted relative Mann-Whitney effects with ordinary ranks or 
#' unweighted relative Mann-Whitney effects with pseudo-ranks shall be used. Weighted related effects depends on sample sizes and their allocations, whereas
#' unweighted relative effects do not. In case of a balanced design (equal sample sizes), both effects are identical. 
#' @param cell.weights Character string specifying how dependent replicates are 
#' weighted. Use \code{"subjects"} for subject-level cell weighting and 
#' \code{"observations"} for observation-level cell weighting. 
#' @param contrast Optional list specifying a factor or interaction for multiple 
#' contrast procedures.
#' @param sci.method Method for the computation of simultaneous confidence intervals. The Fisher-transformation is default.
#' @param alpha Significance level for tests and confidence intervals. 
#' @param CI.method Method used for confidence intervals for relative effects. 
#' @param covariance Logical. Should the estimated covariance matrix be returned? The default is \code{FALSE}
#' @param Factor.Information Logical. Should factor-specific information be returned? 
#' @param perm.test Logical. Should the paired two-time-point permutation test be 
#' used when applicable? 
#' @param B Number of permutation samples for the computation of the permutation test in paired data. 
#'
#' @details 
#' The function provides rank- and pseudo-rank-based procedures for factorial 
#' longitudinal data. The argument \code{hypothesis = "H0F"} specifies 
#' hypotheses in marginal distribution functions. These hypotheses compare the 
#' complete marginal distributions and may therefore detect differences in 
#' location, scale, skewness, or other distributional features. 
#' The argument \code{hypothesis = "H0p"} specifies hypotheses in unweighted 
#' relative marginal effects. These effects describe the relative position of 
#' each marginal distribution with respect to a common unweighted reference 
#' distribution and are particularly useful for effect interpretation, multiple 
#' contrast procedures, simultaneous confidence intervals, and graphical 
#' summaries. 
#' 
#' Missing observations are allowed. Incomplete subject-condition cells 
#' contribute where observations are available, and covariance estimation is 
#' based on the independent subject-level units. Dependent replicate 
#' measurements can be specified using the \code{replicate} argument. For 
#' hypotheses in relative marginal effects, \code{cell.weights = "subjects"} 
#' targets a typical subject-condition cell, whereas 
#' \code{cell.weights = "observations"} targets a typical replicate observation. 
#' For hypotheses in marginal distribution functions, dependent replicates are 
#' handled by averaging rank or pseudo-rank scores within subject-condition cells. 
#'
#'
#' @return
#' An object of class \code{"nparld_fit"}. The object is a list containing the
#' results of the nonparametric longitudinal analysis. The main components are:
#' \itemize{
#'   \item \code{Design}: character string describing the detected longitudinal
#'     factorial design.
#'   \item \code{wholeplots}: names of the whole-plot factors.
#'   \item \code{subplots}: names of the subplot or repeated-measures factors.
#'   \item \code{text.ranks}: character string describing the type of ranks used.
#'   \item \code{text.hypotheses}: character string describing the type of
#'     hypotheses tested.
#'   \item \code{N.info}: information on the number of subjects and observations.
#'   \item \code{effects}: a data frame with estimated relative effects. This
#'     includes the factor-level combinations, the number of contributing
#'     subjects and observations, the number of missing observations, the mean
#'     rank or pseudo-rank score, the estimated relative treatment effect, and
#'     its standard error. For \code{hypothesis = "H0p"}, confidence limits are
#'     also returned.
#'   \item \code{factor.info}: optional factor-specific information, returned
#'     when \code{Factor.Information = TRUE}.
#'   \item \code{WTS}: Wald-type test statistics, degrees of freedom, and
#'     p-values for the tested main effects and interactions.
#'   \item \code{ATS}: ANOVA-type test statistics, denominator degrees of
#'     freedom, and p-values for the tested main effects and interactions.
#'   \item \code{MCTP}: multiple contrast test results, returned when
#'     \code{contrast} is specified. These include the selected factor or
#'     interaction, the simultaneous confidence interval method, the global
#'     multiple contrast test, and local contrast estimates with standard errors,
#'     simultaneous confidence limits, test statistics, p-values, and degrees of
#'     freedom.
#'   \item \code{covariance.info}: estimated covariance matrix used for the
#'     selected hypothesis, returned when \code{covariance = TRUE}.
#'   \item \code{perm}: permutation test results, returned when a permutation
#'     test is requested and applicable.
#'   \item \code{hypothesis}: the type of hypothesis tested, either \code{"H0F"}
#'     for hypotheses in marginal distribution functions or \code{"H0p"} for
#'     hypotheses in unweighted relative marginal effects.
#'   \item \code{CI.method}: method used for confidence intervals.
#'   \item \code{.internal}: internal objects used for computation and advanced
#'     post-processing.
#' }
#'
#' Some components may be \code{NULL} depending on the selected hypothesis,
#' contrast specification, and output options.
#'
#'
#'
#' @examples
#' set.seed(123)
#' dat <- data.frame(
#'   subject = rep(1:10, each = 4),
#'   time = rep(1:4, times = 10),
#'   y = rnorm(40)
#' )
#' fit <- nparLD(y ~ time, data = dat, subject = "subject",
#'               hypothesis = "H0p")
#' fit
#'
#'
#' @references 
#'
#' Akritas, M. G., & Brunner, E. (1997). A unified approach to rank tests for mixed models. 
#' Journal of Statistical Planning and Inference, 61(2), 249-277.
#'
#' Brunner, E., Bathke, A.C., Konietschke, F. Rank and Pseudo-Rank Procedures 
#' for Independent Observations in Factorial Designs. Springer International Publishing, 2018.
#'
#' Brunner, E., Domhof, S., & Langer, F. (2002). Nonparametric analysis of longitudinal data in factorial experiments. Wiley, New York
#'
#' Brunner, E., Konietschke, F., Pauly, M., Puri, M. L. (2017). Rank-based procedures in factorial designs: 
#' Hypotheses about non-parametric treatment effects. Journal of the Royal Statistical Society: Series B 
#' (Statistical Methodology), 79(5), 1463-1485.
#' 
#' Domhof, S., Brunner, E., Osgood, D. W. (2002). Rank procedures for repeated measures with missing values. 
#' Sociological methods & research, 30(3), 367-393.
#' 
#' Konietschke, F., Bathke, A. C., Hothorn, L. A., & Brunner, E. (2010). 
#' Testing and estimation of purely nonparametric effects in repeated measures designs. 
#' Computational Statistics & Data Analysis, 54(8), 1895-1905.
#'
#'Konietschke, F., Hothorn, L. A., Brunner, E. (2012). Rank-based multiple test procedures and simultaneous confidence intervals. Electronic Journal of Statistics,
#' 6, 738-759.
#' 
#' Rubarth, K., Pauly, M., & Konietschke, F. (2022). Ranking procedures for repeated measures designs with missing data: 
#' estimation, testing and asymptotic theory. 
#' Statistical Methods in Medical Research, 31(1), 105-118.
#'
#' Rubarth, K., Sattler, P., Zimmermann, H. G., & Konietschke, F. (2021). 
#' Estimation and testing of Wilcoxon–Mann–Whitney effects in factorial clustered data designs. 
#' Symmetry, 14(2), 244.
#' 
#'
#'
#' @export 

nparLD <- function(formula, data, alpha = 0.05, subject, replicate = NULL,
                   CI.method = c("logit", "normal"),
                   effect = c("unweighted", "weighted"),
                   cell.weights = c("subjects", "observations"),
                   hypothesis = c("H0F", "H0p"),
                   contrast = NULL,
                   sci.method = c("fisher", "multi.t"),
                   Factor.Information = FALSE,
                   covariance = FALSE,
                   perm.test = FALSE,
                   B = 1000) {

effect    <- match.arg(effect)
cell.weights <- match.arg(cell.weights)
hypothesis   <- match.arg(hypothesis)
CI.method    <- match.arg(CI.method)
sci.method   <- match.arg(sci.method)
if (cell.weights %in% c("nu1", "subjects")) cell.weights <- "subjects"
if (cell.weights %in% c("nu2", "observations")) cell.weights <- "observations"

  #*******************************************************************************
  # Factor Design
  #*******************************************************************************
  design <- .nparld_build_design(formula, data, subject, replicate)

  dat <- design$dat
  formula <- design$formula
  replicate_internal <- if (!is.null(replicate)) "replicate" else NULL

.nparld_check_complete_within_schedule(
  dat,
  design$SP.names,
  replicate = replicate_internal
)

.check_replicate_structure(
  dat = dat,
  WP.names = design$WP.names,
  SP.names = design$SP.names,
  replicate = replicate_internal
)
  #*******************************************************************************
  # Add Ranks or Pseudo-Ranks to the data set
  #*******************************************************************************
  split_obj <- .nparld_split_by_wp_subject_sp(
    dat,
    design$WP.names,
    design$SP.names
  )

  Fhat <- NULL
  Ghat <- NULL

  if (effect == "weighted") {
    dat$Ranks <- rank(dat[[1]], na.last = "keep")
    text.ranks <- "Classical ranks (mid-ranks)"
  }

  if (effect == "unweighted" || hypothesis == "H0p") {
    Fhat <- .nparld_build_Fhat(
      split_obj,
      cell.weights = cell.weights
    )

Ghat <- .nparld_build_Ghat(
  split_obj = split_obj,
  Fhat = Fhat,
  effect = effect,
  cell.weights = cell.weights
)

  Ntotal <- sum(!is.na(dat[[1]]))
dat <- .nparld_compute_pseudoranksN(dat, Ghat, Ntotal)
    text.ranks <- "Pseudo-ranks"
  }
est <- .nparld_estimate_theta_rankmeans(
  dat = dat,
  formula = formula,
  WP.names = design$WP.names,
  SP.names = design$SP.names,
  replicate = replicate_internal,
  effect = effect,
  cell.weights = cell.weights
)

  dat <- est$dat
  effects <- est$effects
  theta_hat <- est$theta_hat
  Ntotal <- est$Ntotal

  #*******************************************************************************
  # Compute the covariance matrix accordingly
  #*******************************************************************************
  hy <- .nparld_build_hypotheses(
    formula,
    design$factor_cols,
    design$n.levels
  )

  res.mctp <- NULL
  Sigma <- NULL
  df_satt <- NULL

  if (hypothesis == "H0p") {

    if (effect == "weighted") {
      stop("Testing H0p with weighted reference distribution is not implemented.",
           call. = FALSE)
    }

    if (!is.null(contrast)) {
      if (!any(hy$Output.names == contrast[[1]])) {
        stop("Factor not found in model.", call. = FALSE)
      }

      pos <- which(hy$Output.names == contrast[[1]])
      H <- hy$Hypotheses[[pos]]
      C <- .nparld_build_contrast_matrix(contrast, H)
      CH <- C %*% H

      if (cell.weights == "subjects") {
        covp <- .nparld_cov_H0p_ref_unweighted_subjects(
          split_obj,
          Fhat,
          C = CH
        )
      } else {
        covp <- .nparld_cov_H0p_ref_unweighted_observations(
          split_obj,
          Fhat,
          C = CH
        )
      }

      Sigma   <- covp$Sigma
      df_satt <- covp$df_satt

    } else {

      if (cell.weights == "subjects") {
        covp <- .nparld_cov_H0p_ref_unweighted_subjects(
          split_obj,
          Fhat,
          C = NULL
        )
      } else {
        covp <- .nparld_cov_H0p_ref_unweighted_observations(
          split_obj,
          Fhat,
          C = NULL
        )
      }

      Sigma   <- covp$Sigma
      df_satt <- covp$df_satt
    }

    if (is.null(Sigma)) {
      stop("Sigma could not be computed.", call. = FALSE)
    }

if (hypothesis == "H0p") {
  var_theta <- diag(Sigma)

  eps_diag <- diag(covp$eps_theta)

  ## diag(eps_theta) is NA by construction, because eps_theta is pairwise.
  ## For individual effects, use the smallest finite pairwise floor involving
  ## the corresponding cell.
  eps_effect <- apply(covp$eps_theta, 1, function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0L) NA_real_ else min(x)
  })

  var_theta <- .nparld_regularize_theta_variance(
    var_theta = var_theta,
    eps_theta = eps_effect
  )

  Sigma.reg <- Sigma
  diag(Sigma.reg) <- var_theta

  effects$std.error <- sqrt(diag(Sigma.reg))

  effects <- cbind(
    effects,
    .nparld_limits(theta_hat, Sigma.reg, alpha, design$N, CI.method)
  )

  Sigma <- Sigma.reg
}

    text.hypotheses <- "Tested in marginal Mann-Whitney effects (marginal relative effects)"
  }

  if (hypothesis == "H0F") {

    if (!is.null(replicate)) {
      message("For H0F, the ranks (or pseudo-ranks) of the replicate observations are averaged within subject-cells.")
    }

    Sigma <- .nparld_estimate_sigma(
      dat = dat,
      WP.names = design$WP.names,
      SP.names = design$SP.names,
      N = design$N,
      Ntotal = Ntotal
    )

    effects$std.error <- sqrt(diag(Sigma))
    text.hypotheses <- "Tested in distribution functions"
  }

  #*******************************************************************************
  # Compute the test statistics
  #*******************************************************************************
  tests <- .nparld_run_tests(
    theta_hat,
    Sigma,
    hy$Hypotheses,
    design$N,
    hy$Output.names
  )

  if (!is.null(contrast)) {
    pos <- which(hy$Output.names == contrast[[1]])
    H <- hy$Hypotheses[[pos]]
    C <- .nparld_build_contrast_matrix(contrast, H)

    # NOTE: sci.method must exist elsewhere in your codebase
res.mctp <- .nparld_run_contrasts(
  theta_hat = theta_hat,
  Sigma = Sigma,
  H = H,
  C = C,
  N = design$N,
  alpha = alpha,
  sci.method = sci.method,
  hypothesis = hypothesis,
  df_satt = df_satt,
  eps_theta = if (hypothesis == "H0p") covp$eps_theta else NULL,
  factor_name = contrast[[1]]
)
  }

  #*******************************************************************************
  # Factor information
  #*******************************************************************************
  factor.info <- NULL

  if (Factor.Information) {
    factor.info <- .nparld_run_factor.information(
      theta_hat = theta_hat,
      Sigma = Sigma,
      CIMatrices = hy$CI.Matrices,
      Output.names = hy$Output.names,
      names.levels = design$names.levels,
      alpha = alpha,
      N = design$N,
      CI.method = CI.method,
      include.CI = (hypothesis == "H0p")
    )
  }

  #*****************************************************************************
  # Return the Covariance Matrix estimator
  #*****************************************************************************

covariance.info <- NULL


if (covariance) {
  covariance.info <- Sigma
  class(covariance.info) <- c("nparld_covarianceinfo", class(Sigma))
}
  

  #*******************************************************************************
  # Two time points
  #*******************************************************************************
  perm_res <- list(available = FALSE)

  if (perm.test) {
    design_obj <- .nparld_build_design(formula, data, subject, replicate)

    if (.can_use_perm_test(design_obj)) {
      samp <- .extract_paired_samples(design_obj)

      res_perm_raw <- studentized_perm_test_paired(
        samp$x, samp$y,
        B = B,
        alpha = alpha,
        hypothesis = hypothesis
      )

      table_out <- .format_perm_output(
        res = res_perm_raw,
        alpha = alpha,
        hypothesis = hypothesis
      )

      perm_res <- list(
        available = TRUE,
        method = "Studentized permutation test (paired)",
        table = table_out,
        raw = res_perm_raw,
        levels = samp$lev
      )

    } else {
      perm_res <- list(
        available = FALSE,
        message = "Permutation test not applicable (requires 1 subplot factor with 2 levels and complete data)."
      )
    }
  }

  #*******************************************************************************
  # Final result object
  #*******************************************************************************
  result <- list(
    Design = design$Design,
    wholeplots = design$wholeplots,
    subplots = design$subplots,
     text.ranks = text.ranks,
      text.hypotheses = text.hypotheses,
    N.info = design$N.info,
    effects = effects,
    factor.info = factor.info,
    WTS = tests$WTS,
    ATS = tests$ATS,
    MCTP = res.mctp,
    perm = perm_res,
    hypothesis = hypothesis,
    CI.method = CI.method,
    covariance.info = covariance.info,
    .internal = list(
      formula = formula,
      WP.names = design$WP.names,
      SP.names = design$SP.names,
      factor_cols = design$factor_cols,
      sci.method = sci.method,
      n.levels = design$n.levels,
      names.levels = design$names.levels,
      N = design$N,
      Sigma = Sigma,
      Hypotheses = hy$Hypotheses,
      CI.Matrices = hy$CI.Matrices,
      Output.names = hy$Output.names,
      effect = effect,
      cell.weights = cell.weights,
      hypothesis = hypothesis,
      CI.method = CI.method,
      alpha = alpha,
      Factor.Information = Factor.Information
    )
  )

  class(result) <- "nparld_fit"
  result
}

