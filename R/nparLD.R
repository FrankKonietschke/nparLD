#' Nonparametric analysis of longitudinal data
#'
#' Performs nonparametric inference for longitudinal or repeated-measures data
#' from crossed factorial experiments. The function can be used to test hypotheses in
#' marginal distribution functions or hypotheses in unweighted relative
#' marginal effects. It allows crossed whole-plot and sub-plot factors,
#' missing values, and dependent replicate measurements.
#'
#' @param formula A model formula of the form \code{response ~ factors}. Factors
#'   on the right-hand side may contain crossed whole-plot and subplot factors
#'   and their interactions.
#' @param data A data frame containing the response, subject variable, design
#'   factors, and optionally a replicate variable.
#' @param subject Character string specifying the subject identifier.
#' @param replicate Optional character string specifying the replicate identifier.
#'   Replicates can be dependent within subject-condition cells.
#' @param cell.weights Character string specifying how dependent replicates are
#'   weighted. Use \code{"subjects"} for subject-level cell weighting and
#'   \code{"observations"} for observation-level cell weighting. 
#'   For dependent replicates under \code{hypothesis = "H0F"},
#' \code{cell.weights = "subjects"} averages rank scores within
#' subject-condition cells before covariance estimation, whereas
#' \code{cell.weights = "observations"} uses the sums of the observed rank
#' scores within subject-condition cells and therefore gives more weight to
#' subjects with more observed replicate measurements.The default is
#'   \code{"subjects"}.
#' @param hypothesis Character string specifying the hypothesis type. Use
#'   \code{"H0F"} for hypotheses in marginal distribution functions and
#'   \code{"H0p"} for hypotheses in unweighted relative marginal effects.
#'   The latter permits unequal variances or higher-order distributional
#'   differences under the null hypothesis and is related to the nonparametric
#'   Behrens-Fisher problem.
#' @param effect Character string specifying whether weighted or unweighted
#'   relative effects are used. Use \code{"weighted"} for effects based on
#'   ordinary ranks and a sample-size weighted reference distribution. Use
#'   \code{"unweighted"} for effects based on pseudo-ranks and an equally
#'   weighted factorial-cell reference distribution. Weighted effects depend on
#'   the sample-size allocation and are mainly descriptive. Unweighted effects
#'   are the inferential target for \code{hypothesis = "H0p"}. In balanced
#'   designs, both effect definitions coincide. The default is
#'   \code{effect = "unweighted"}.
#' @param contrast Optional list specifying a factor or interaction term for
#'   simultaneous inference. A one-element specification such as
#'   \code{list("time")} uses the standard hypothesis matrix for the selected
#'   model term, that is, the same contrast structure underlying the WTS and ATS
#'   for that term. A two-element specification such as
#'   \code{list("time", "Tukey")} or \code{list("time", "Dunnett")} applies the
#'   named multiple contrast procedure to the marginal relative effects of the
#'   selected term. A numeric vector or matrix may be supplied as the second
#'   element for user-defined contrasts.
#' @param sci.method Character string specifying the method for simultaneous
#'   confidence intervals. Available choices are \code{"fisher"} and \code{"multi.t"}. 
#' The default is \code{"fisher"}.
#' @param Factor.Information Logical. If \code{TRUE}, factor-specific relative
#'   effects, standard errors, and confidence intervals are returned. These
#'   summaries can also be displayed with \code{plot(fit, term = ...)}.
#' @param alpha Significance level for tests and confidence intervals. The
#'   default is \code{alpha = 0.05}.
#' @param CI.method Character string specifying the method for confidence intervals for 
#' relative effects. Available choices are \code{"logit"} and \code{"normal"}. The default is \code{"logit"}.
#' @param covariance Logical. If \code{TRUE}, the estimated covariance matrix
#'   used for the selected hypothesis is included in the output. The default is
#'   \code{FALSE}.
#' @param perm.test Logical. Should the paired studentized permutation test be
#'   used when applicable? This option is only available for paired designs with
#'   two time points.
#' @param B Number of permutation samples used for the paired permutation test.
#'
#' @details
#' The function provides rank- and pseudo-rank-based procedures for factorial
#' longitudinal data. The argument \code{hypothesis = "H0F"} specifies
#' hypotheses in marginal distribution functions. These hypotheses compare the
#' complete marginal distributions and are tested by rank-based procedures.
#'
#' The argument \code{hypothesis = "H0p"} specifies hypotheses in unweighted
#' relative marginal effects. These effects describe the relative position of
#' each marginal distribution with respect to a common unweighted reference
#' distribution and are particularly useful for effect interpretation, multiple
#' contrast procedures, simultaneous confidence intervals, and graphical
#' summaries.
#'
#' The \code{contrast} argument computes a multiple contrast test along with simultaneous
#' confidence intervals for the selected model terms. A one-element specification such as 
#' \code{list("time")} uses the same hypothesis matrix as the global test procedures (Wald-
#' and ANOVA-type statistics) for the selected term. This
#' provides simultaneous inference for the components of the corresponding
#' global null hypothesis. A two-element specification such as
#' \code{list("time", "Tukey")} or \code{list("time", "Dunnett")} first forms
#' the marginal relative effects for the selected term and then applies the
#' requested multiple contrast procedure. Thus, \code{list("time", "Tukey")}
#' gives pairwise comparisons of the marginal time effects, whereas
#' \code{list("time", "Dunnett")} compares the marginal time effects with the
#' first time point. User-defined contrast vectors or matrices can be supplied
#' as the second list element.
#'
#' Missing observations are allowed. Incomplete subject-condition cells
#' contribute where observations are available, and covariance estimation is
#' based on the independent subject-level units. Dependent replicate
#' measurements can be specified using the \code{replicate} argument. For
#' hypotheses in relative marginal effects, \code{cell.weights = "subjects"}
#' targets a typical subject-condition cell, whereas
#' \code{cell.weights = "observations"} targets a typical replicate observation.
#' For hypotheses in marginal distribution functions, dependent replicates are
#' handled by averaging rank or pseudo-rank scores within subject-condition
#' cells.

#' In case of bivariate data (e.g., before and after measurements), the function implements
#' a studentized permutation test for testing either \code{hypothesis = "H0F"} or
#' \code{hypothesis = "H0p"}. The reported effect estimate is the pairwise
#' Mann--Whitney effect \eqn{\int F_1,dF_2}, computed directly from the two
#' marginal distributions and therefore without introducing a common reference
#' distribution. For more details, see Konietschke and Pauly (2012).
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
#'     rank or pseudo-rank score, the estimated relative effect, and its
#'     standard error. For \code{hypothesis = "H0p"}, confidence limits are also
#'     returned.
#'   \item \code{factor.info}: optional factor-specific relative effects,
#'     standard errors, and confidence limits for main effects and interactions,
#'     returned when \code{Factor.Information = TRUE}. These summaries can be
#'     displayed with \code{plot(fit, term = ...)}.
#'   \item \code{WTS}: Wald-type test statistics, degrees of freedom, and
#'     p-values for the tested main effects and interactions.
#'   \item \code{ATS}: ANOVA-type test statistics, denominator degrees of
#'     freedom, and p-values for the tested main effects and interactions.
#'   \item \code{MCTP}: multiple contrast test results, returned when
#'     \code{contrast} is specified. These include the selected factor or
#'     interaction, the global multiple contrast test, local contrast estimates
#'     with standard errors, test statistics, p-values, degrees of freedom, and
#'     the corresponding contrast matrix. For \code{hypothesis = "H0p"},
#'     simultaneous confidence limits and the selected SCI method are also
#'     returned.
#'   \item \code{covariance.info}: estimated asymptotic covariance matrix of
#'     \eqn{\sqrt{N}(\hat{\theta} - \theta)} used for the selected hypothesis,
#'     returned when \code{covariance = TRUE}, where \eqn{N} denotes the number
#'     of independent subjects.
#'   \item \code{perm}: Permutation test results, returned when a permutation
#'   test is requested and applicable. The output contains inference based on
#'   the \eqn{t_{n-1}} distribution (suffix \code{.t}) and on the studentized
#'   permutation distribution (suffix \code{.perm}).
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
#' @seealso
#' \code{\link{WTS}}, \code{\link{ATS}},
#' \code{\link{MCTP}}, \code{\link{factorInfo}},
#' \code{\link{covariance}}, \code{\link{permTest}}
#'
#' @examples
#' ## One repeated-measures factor: dental data
#' data(dental)
#'
#' fit_dental <- nparLD(
#'   resp ~ time,
#'   data = dental,
#'   subject = "subject",
#'   hypothesis = "H0p"
#' )
#'
#' fit_dental
#' effects(fit_dental)
#'
#' ## Crossed factorial longitudinal design: shoulder data
#' data(shoulder)
#'
#' \donttest{
#' fit_shoulder <- nparLD(
#'   resp ~ group1 * group2 * time,
#'   data = shoulder,
#'   subject = "subject",Factor.Information=TRUE,
#'   hypothesis = "H0p"
#' )
#'
#' fit_shoulder
#' effects(fit_shoulder)
#' factorInfo(fit_shoulder)
#' }
#'
#' ## Multiple contrast procedure for an interaction
#'
#' \donttest{
#' fit_contrast <- nparLD(
#'   resp ~ group1 * group2 * time,
#'   data = shoulder,
#'   subject = "subject",
#'   hypothesis = "H0p",
#'   contrast = list("group1:time")
#' )
#'
#' MCTP(fit_contrast)
#' print(MCTP(fit_contrast), show.matrix = TRUE)
#' }
#' 
#' ## Missing response values
#' \donttest{
#' dat_miss <- dental
#' dat_miss$resp[c(2, 7, 12)] <- NA
#'
#' fit_miss <- nparLD(
#'   resp ~ time,
#'   data = dat_miss,
#'   subject = "subject",
#'   hypothesis = "H0p"
#' )
#'
#' fit_miss
#' }
#'
#' ## Dependent replicate measurements: BrdU data
#' data(brdu)
#'
#' \donttest{
#' fit_brdu <- nparLD(
#'   resp ~ dose,
#'   data = brdu,
#'   subject = "culture",
#'   replicate = "replicate",
#'   hypothesis = "H0p",
#'   cell.weights = "subjects"
#' )
#'
#' fit_brdu
#' effects(fit_brdu)
#'}
#'
#' \donttest{
#' ## Graphical displays
#' plot(fit_dental)
#' plot(fit_shoulder)
#' plot(MCTP(fit_contrast))
#' }
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
#' Konietschke, F., Hothorn, L. A., Brunner, E. (2012). Rank-based multiple test procedures and simultaneous confidence intervals. Electronic Journal of Statistics,
#' 6, 738-759.
#' 
#' Konietschke, F., Pauly, M. (2012). A studentized permutation test for the nonparametric Behrens-Fisher problem in paired data. Electronic Journal of Statistics, 
#' 6, 1358 - 1372.
#' 
#' Rubarth, K., Pauly, M., & Konietschke, F. (2022). Ranking procedures for repeated measures designs with missing data: 
#' estimation, testing and asymptotic theory. 
#' Statistical Methods in Medical Research, 31(1), 105-118.
#'
#' Rubarth, K., Sattler, P., Zimmermann, H. G., & Konietschke, F. (2021). 
#' Estimation and testing of Wilcoxon-Mann-Whitney effects in factorial clustered data designs. 
#' Symmetry, 14(2), 244.
#' 
#'
#'
#' @export 

nparLD <- function(formula, data, subject, replicate = NULL,
                   cell.weights = c("subjects", "observations"),
                   effect = c("unweighted", "weighted"),
                   hypothesis = c("H0F", "H0p"),
                   contrast = NULL,
                   sci.method = c("fisher", "multi.t"),
                   Factor.Information = FALSE,
                   CI.method = c("logit", "normal"),
                   alpha = 0.05,
                   covariance = FALSE,
                   perm.test = FALSE,
                   B = 1000) {

  effect <- match.arg(effect)
  cell.weights <- match.arg(cell.weights)
  hypothesis <- match.arg(hypothesis)
  CI.method <- match.arg(CI.method)
  sci.method <- match.arg(sci.method)

  if (cell.weights %in% c("nu1", "subjects")) {
    cell.weights <- "subjects"
  }

  if (cell.weights %in% c("nu2", "observations")) {
    cell.weights <- "observations"
  }


  #*******************************************************************************
  # Factor Design
  #*******************************************************************************

  design <- .nparld_build_design(
    formula,
    data,
    subject,
    replicate
  )

  dat <- design$dat
  identical_cells <- .nparld_find_identical_cells(
  dat,
  design$factor_cols
)

if (length(identical_cells) > 0L) {
  message(
    "Identical cell response vectors detected:\n",
    paste0("  ", identical_cells, collapse = "\n")
  )
}

  formula <- design$formula

  replicate_internal <- if (!is.null(replicate)) {
    "replicate"
  } else {
    NULL
  }

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

    dat$Ranks <- rank(
      dat[[1]],
      na.last = "keep"
    )

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

    Ntotal <- sum(
      !is.na(dat[[1]])
    )

    dat <- .nparld_compute_pseudoranksN(
      dat,
      Ghat,
      Ntotal
    )

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
  # Compute hypothesis matrices
  #*******************************************************************************

  hy <- .nparld_build_hypotheses(
    formula,
    design$factor_cols,
    design$n.levels
  )

  res.mctp <- NULL
  Sigma <- NULL
  df_satt <- NULL

  H <- NULL
  C <- NULL


  #*******************************************************************************
  # Prepare multiple contrast procedure
  #*******************************************************************************

  if (!is.null(contrast)) {

    if (!any(hy$Output.names == contrast[[1]])) {
      stop(
        "Factor not found in model.",
        call. = FALSE
      )
    }

    pos <- which(
      hy$Output.names == contrast[[1]]
    )

    if (length(pos) != 1L) {
      stop(
        "Requested contrast term is ambiguous.",
        call. = FALSE
      )
    }

    if (length(contrast) == 1L) {

      ## Default MCTP:
      ## use the same hypothesis matrix as WTS/ATS

      H <- hy$Hypotheses[[pos]]

      C <- diag(
        nrow(H)
      )

      rownames(C) <- rownames(H)

    } else {

      ## Named or user-defined contrasts:
      ## apply contrasts to marginal effects

      H <- hy$CI.Matrices[[pos]]
      C <- .nparld_build_contrast_matrix(
        contrast,
        H
      )
    }
  }


  #*******************************************************************************
  # H0p covariance estimation
  #*******************************************************************************

  if (hypothesis == "H0p") {

    if (effect == "weighted") {
      stop(
        paste(
          "Testing H0p with weighted reference",
          "distribution is not implemented."
        ),
        call. = FALSE
      )
    }

    if (!is.null(contrast)) {

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
    }

    Sigma <- covp$Sigma
    df_satt <- covp$df_satt

    if (is.null(Sigma)) {
      stop(
        "Sigma could not be computed.",
        call. = FALSE
      )
    }


    #---------------------------------------------------------------------------
    # Regularize zero/non-finite marginal variances
    #---------------------------------------------------------------------------

    var_theta <- diag(Sigma)

    eps_effect <- apply(
      covp$eps_theta,
      1,
      function(x) {

        x <- x[
          is.finite(x)
        ]

        if (length(x) == 0L) {
          NA_real_
        } else {
          min(x)
        }
      }
    )

    var_theta <- .nparld_regularize_theta_variance(
      var_theta = var_theta,
      eps_theta = eps_effect
    )

    Sigma.reg <- Sigma
    diag(Sigma.reg) <- var_theta

    effects$Std.Error <- sqrt(
      diag(Sigma.reg)/design$N
    )

    effects <- cbind(
      effects,
      .nparld_limits(
        theta_hat,
        Sigma.reg,
        alpha,
        design$N,
        CI.method
      )
    )

    Sigma <- Sigma.reg

    text.hypotheses <-
      "Tested in marginal Mann-Whitney effects"
  }


  #*******************************************************************************
  # H0F covariance estimation
  #*******************************************************************************

  if (hypothesis == "H0F") {

    Sigma <- .nparld_estimate_sigma(
      dat = dat,
      WP.names = design$WP.names,
      SP.names = design$SP.names,
      N = design$N,
      Ntotal = Ntotal,
      replicate = replicate,
      cell.weights = cell.weights
    )

    effects$Std.Error <- sqrt(
      diag(Sigma)/design$N
    )

    text.hypotheses <-
      "Tested in distribution functions"
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


  #*******************************************************************************
  # Multiple contrast procedure
  #*******************************************************************************

  if (!is.null(contrast)) {

    effect_labels <- .nparld_cell_keys(
      design$dat,
      design$WP.names,
      design$SP.names
    )

    contrast_type <- if (length(contrast) == 1L) {

  "Default"

} else if (is.character(contrast[[2]])) {

  contrast[[2]]

} else {

  "User-defined"
}

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
  eps_theta = if (hypothesis == "H0p") {
    covp$eps_theta
  } else {
    NULL
  },
  factor_name = contrast[[1]],
  contrast_type = contrast_type,
  effect_labels = effect_labels
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

    class(covariance.info) <- c(
      "nparld_covarianceinfo",
      class(Sigma)
    )
  }


  #*******************************************************************************
  # Two time points
  #*******************************************************************************

  perm_res <- list(
    available = FALSE
  )

  if (perm.test) {

    design_obj <- .nparld_build_design(
      formula,
      data,
      subject,
      replicate
    )

    if (.can_use_perm_test(design_obj)) {

      samp <- .extract_paired_samples(
        design_obj
      )

      res_perm_raw <- studentized_perm_test_paired(
        samp$x,
        samp$y,
        B = B,
        alpha = alpha,
        hypothesis = hypothesis
      )

  table_out <- .format_perm_output(
  res = res_perm_raw,
  alpha = alpha,
  hypothesis = hypothesis,
  factor_name = samp$factor
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
        message = paste(
          "Permutation test not applicable",
          "(requires 1 subplot factor with 2 levels",
          "and complete data)."
        )
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