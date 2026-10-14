#*******************************************************************************
# Output Functions
#*******************************************************************************

utils::globalVariables(c(".data", "RTE", "lower", "upper"))

.nparld_pvalue_columns <- function(tab) {
  nms <- names(tab)
  low <- tolower(nms)

  low %in% c("p-value", "p.value", "pval", "p") |
    grepl("^p\\.", low) |
    grepl("^p_", low) |
    grepl("p-value$", low) |
    grepl("p.value$", low)
}

.nparld_print_table <- function(tab, digits = 4, eps = 1e-4, ...) {
  tab <- as.data.frame(tab)

  p_cols <- .nparld_pvalue_columns(tab)
  num_cols <- vapply(tab, is.numeric, logical(1))

  round_cols <- num_cols & !p_cols
  tab[round_cols] <- lapply(tab[round_cols], round, digits = digits)

  for (v in names(tab)[p_cols]) {
    tab[[v]] <- format.pval(tab[[v]], digits = digits, eps = eps)
  }

  print(tab, ...)
  invisible(tab)
}

#' Print an nparLD fit
#'
#' Prints a concise overview of a fitted \code{nparLD()} model.
#'
#' @param x An object of class \code{"nparld_fit"}.
#' @param digits Number of digits used for printed numerical results.
#' @param ... Further arguments.
#'
#' @details
#' The print method provides a compact summary of the fitted model, including
#' the detected design, whole-plot and subplot factors, number of subjects,
#' hypothesis type, ranking method, and, for effect-based inference, the
#' confidence interval method.
#'
#' The inferential output is intentionally restricted to the ANOVA-type
#' statistic (ATS) in order to keep the standard print output concise.
#' More detailed results, including estimated relative effects, factor-specific
#' summaries, Wald-type statistics, multiple contrast procedures, and
#' permutation-test results when available, are provided by
#' \code{summary()}. Individual result components can also be extracted using
#' the corresponding accessor functions.
#'
#' If a multiple contrast procedure was requested during model fitting, the
#' print method indicates that the corresponding results are available through
#' \code{MCTP()}.
#'
#' @export
print.nparld_fit <- function(x, digits = 4, ...) {

  cat("\nNonparametric Longitudinal Analysis\n")
  cat("-------------------------------------------------\n")

  if (!is.null(x$Design)) {
    cat("Design: ", x$Design, "\n", sep = "")

    if (!is.null(x$wholeplots) && length(x$wholeplots) > 0L) {
      cat(
        "Whole-plot factors: ",
        paste(x$wholeplots, collapse = ", "),
        "\n",
        sep = ""
      )
    }

    if (!is.null(x$subplots) && length(x$subplots) > 0L) {
      cat(
        "Subplot factors: ",
        paste(x$subplots, collapse = ", "),
        "\n",
        sep = ""
      )
    }
  }

  if (!is.null(x$N.info)) {
    cat("Number of subjects: ", x$N.info, "\n", sep = "")
  }

  if (!is.null(x$text.hypotheses)) {
    cat("Hypotheses: ", x$text.hypotheses, "\n", sep = "")
  }

  if (!is.null(x$text.ranks)) {
    cat("Ranking: ", x$text.ranks, "\n", sep = "")
  }

  if (!is.null(x$hypothesis) &&
      identical(x$hypothesis, "H0p") &&
      !is.null(x$CI.method)) {
    cat(
      "Confidence interval method: ",
      x$CI.method,
      "\n",
      sep = ""
    )
  }

  if (!is.null(x$ATS)) {
    cat("\nANOVA-type statistic (ATS)\n")
    cat("-------------------------------------------------\n")
    .nparld_print_table(x$ATS, digits = digits)
  }

  if (!is.null(x$MCTP)) {
  cat("\nMultiple contrast results available via MCTP(x).\n")
}

cat("\nUse summary(x) for detailed results.\n")


  invisible(x)
}

#' Print nparLD covariance matrix
#'
#' Displays the estimated covariance matrix of the relative effect estimator. 
#'
#' @param x An object of class \code{"nparld_covarianceinfo"}.
#' @param digits Number of digits used for printing numerical results.
#' @param ... Further arguments.
#'
#' @details
#' The printed matrix is the estimated covariance matrix used for the selected
#' hypothesis. It is returned when \code{covariance = TRUE}.
#' @export
print.nparld_covarianceinfo <- function(x, digits = 4, ...) {

  cat("\nEstimated covariance matrix\n")
  cat("-------------------------------------------------\n")

  print(round(unclass(x), digits = digits))

  invisible(x)
}

#' Print nparLD factor information
#'
#' Prints factor-specific relative effects, standard errors, and confidence
#' limits for the main effects and interactions of an \code{nparLD()} fit.
#' Factor information is returned when the model is fitted with
#' \code{Factor.Information = TRUE} and can also be displayed graphically with
#' \code{plot(fit, term = ...)}.
#'
#' @param x An object of class \code{"nparld_factorinfo"}.
#' @param digits Number of digits used for printing numerical results.
#' @param ... Further arguments.
#'
#' @details
#' Factor information consists of term-specific relative effects, standard
#' errors, and confidence limits for main effects and interactions. These
#' summaries are returned when \code{Factor.Information = TRUE} and can be
#' visualized with \code{plot(fit, term = ...)}.
#'
#' @export
print.nparld_factorinfo <- function(x, digits = 4, ...) {

  cat("\nFactor-specific relative effects\n")
  cat("-------------------------------------------------\n")

  for (i in seq_along(x)) {

    cat("\nTerm: ", names(x)[i], "\n", sep = "")

    tab <- as.data.frame(x[[i]])

    num_cols <- vapply(tab, is.numeric, logical(1))

    tab[num_cols] <- lapply(tab[num_cols], round, digits = digits)

    print(tab)
  }

  invisible(x)
}

#' Summarize an nparLD fit
#'
#' Produces a detailed summary of a fitted \code{nparLD()} model.
#'
#' @param object An object of class \code{"nparld_fit"}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @details
#' The summary method collects the principal results of the fitted model into a
#' structured object and provides the detailed counterpart to the concise
#' \code{print()} method.
#'
#' The summary includes information on the detected design, whole-plot and
#' subplot factors, number of subjects, hypothesis type, ranking method, and,
#' for effect-based inference, the confidence interval method. It also contains
#' estimated relative effects, factor-specific effect summaries when available,
#' Wald-type and ANOVA-type statistics, multiple contrast results when
#' requested, and permutation-test results when available.
#'
#' The returned object can be printed using the corresponding
#' \code{print.summary.nparld_fit()} method or inspected programmatically.
#' Large auxiliary components, such as covariance matrices, are not included
#' in the printed summary and can be obtained through the corresponding
#' accessor functions.
#'
#' @return An object of class \code{"summary.nparld_fit"} containing the
#'   detailed results of the fitted model.
#'
#' @export
summary.nparld_fit <- function(object, ...) {

  res <- list(
    Design = object$Design,
    wholeplots = object$wholeplots,
    subplots = object$subplots,
    N.info = object$N.info,
    hypothesis = object$hypothesis,
    text.hypotheses = object$text.hypotheses,
    text.ranks = object$text.ranks,
    CI.method = object$CI.method,

    effects = object$effects,
    factor.info = object$factor.info,

    WTS = object$WTS,
    ATS = object$ATS,

    MCTP = object$MCTP,

    covariance.available = !is.null(object$covariance),

    Permutation = if (!is.null(object$perm) &&
                      isTRUE(object$perm$available)) {
      object$perm$table
    } else {
      NULL
    }
  )

  class(res) <- "summary.nparld_fit"
  res
}

#' Print summary of an nparLD fit
#'
#' Prints the detailed summary of a fitted \code{nparLD()} model.
#'
#' @param x An object of class \code{"summary.nparld_fit"}.
#' @param digits Number of digits used for printing numerical results.
#' @param ... Further arguments.
#'
#' @details
#' The summary print method provides a detailed representation of the fitted
#' model. It displays design information, hypothesis and ranking information,
#' estimated relative effects, factor-specific effect summaries when available,
#' and the global Wald-type and ANOVA-type statistics.
#'
#' If additional inferential procedures were requested during model fitting,
#' multiple contrast results and permutation-test results are also printed.
#' Large auxiliary components, such as covariance matrices, are not printed
#' automatically and can be obtained through the corresponding accessor
#' functions.
#'
#' This method is intentionally more detailed than
#' \code{print.nparld_fit()}, which provides a concise overview of the fitted
#' model and the ANOVA-type statistic.
#'
#' @return The summary object is returned invisibly.
#'
#' @export
print.summary.nparld_fit <- function(x, digits = 4, ...) {

  cat("\nSummary of nparLD fit\n")
  cat("-------------------------------------------------\n")

  if (!is.null(x$Design)) {
    cat("Design: ", trimws(x$Design), "\n", sep = "")
  }

  if (!is.null(x$wholeplots) && length(x$wholeplots) > 0L) {
    cat(
      "Whole-plot factors: ",
      paste(x$wholeplots, collapse = ", "),
      "\n",
      sep = ""
    )
  }

  if (!is.null(x$subplots) && length(x$subplots) > 0L) {
    cat(
      "Subplot factors: ",
      paste(x$subplots, collapse = ", "),
      "\n",
      sep = ""
    )
  }

  if (!is.null(x$N.info)) {
    cat("Number of subjects: ", x$N.info, "\n", sep = "")
  }

  if (!is.null(x$text.hypotheses)) {
    cat("Hypotheses: ", x$text.hypotheses, "\n", sep = "")
  } else if (!is.null(x$hypothesis)) {
    cat("Hypothesis: ", x$hypothesis, "\n", sep = "")
  }

  if (!is.null(x$text.ranks)) {
    cat("Ranking: ", x$text.ranks, "\n", sep = "")
  }

  if (!is.null(x$CI.method) &&
      identical(x$hypothesis, "H0p")) {
    cat(
      "Confidence interval method: ",
      x$CI.method,
      "\n",
      sep = ""
    )
  }

  if (!is.null(x$effects)) {
    cat("\nEstimated relative effects\n")
    cat("-------------------------------------------------\n")
    print(x$effects, digits = digits)
  }

  if (!is.null(x$factor.info)) {
    cat("\n")
    print(x$factor.info, digits = digits)
  }

  if (!is.null(x$WTS) || !is.null(x$ATS)) {
    cat("\nGlobal test results\n")
    cat("-------------------------------------------------\n")
  }

  if (!is.null(x$WTS)) {
    cat("\nWald-type statistic (WTS)\n")
    .nparld_print_table(x$WTS, digits = digits)
  }

  if (!is.null(x$ATS)) {
    cat("\nANOVA-type statistic (ATS)\n")
    .nparld_print_table(x$ATS, digits = digits)
  }

  if (!is.null(x$MCTP)) {
    cat("\n")
    print(x$MCTP, digits = digits)
  }

  if (!is.null(x$Permutation)) {
    cat("\nPermutation test (studentized)\n")
    cat("-------------------------------------------------\n")
    .nparld_print_table(x$Permutation, digits = digits)
  }

  if (isTRUE(x$covariance.available)) {
    cat("\nCovariance matrix available via covariance().\n")
  }

  invisible(x)
}

#' Print nparLD multiple contrast results
#'
#' Prints the global and local results of a multiple contrast test procedure.
#' The local results include contrast estimates, standard errors, simultaneous
#' confidence limits, test statistics, adjusted p-values, and degrees of freedom.
#' The contrast matrix can optionally be displayed with
#' \code{show.matrix = TRUE}.
#'
#' @param x An object of class \code{"nparld_mctp"}.
#' @param x An object of class \code{"nparld_mctp"}.
#' @param digits Number of digits used for printing numerical results.
#' @param show.matrix Logical. If \code{TRUE}, print the contrast matrix used in the
#'   multiple contrast procedure.
#' @param ... Further arguments.
#'
#' @details
#' The method prints the global multiple contrast test and the local contrast
#' results, including estimates, standard errors, simultaneous confidence
#' limits, test statistics, adjusted p-values, and degrees of freedom. 
#' The contrast matrix can be displayed with \code{show.matrix = TRUE}
#' or extracted with \code{contrastMatrix()}.
#' @export
print.nparld_mctp <- function(x, digits = 4, show.matrix = FALSE, ...) {

  cat("\nMultiple contrast test procedure\n")
  cat("-------------------------------------------------\n")

  if (!is.null(x$Factor)) {
    cat("Factor or interaction: ", x$Factor, "\n", sep = "")
  }

  if (!is.null(x$sci.method) && !is.na(x$sci.method)) {
    cat("SCI method: ", x$sci.method, "\n", sep = "")
  }

  if (!is.null(x$Global.Result)) {
    cat("\nGlobal result\n")
    cat("-------------------------------------------------\n")
    .nparld_print_table(x$Global.Result, digits = digits)
  }

  if (!is.null(x$Local.Results)) {
    cat("\nLocal contrast results\n")
    cat("-------------------------------------------------\n")
    .nparld_print_table(x$Local.Results, digits = digits)
  }

  if (!is.null(x$Contrast.Matrix)) {
    if (isTRUE(show.matrix)) {
      cat("\nContrast matrix\n")
      cat("-------------------------------------------------\n")
      print(round(x$Contrast.Matrix, digits = digits))
    } else {
      cat("\nContrast matrix not shown by default.\n")
      cat("Use `print(x$MCTP, show.matrix = TRUE)` to display it, or `contrastMatrix(x)` to extract it.\n")
    }
  }

  invisible(x)
}

#' Plot nparLD results
#'
#' Displays graphical summaries of estimated relative effects from an
#' \code{nparLD()} fit.
#'
#' If \code{term} is not supplied, estimated relative effects and confidence
#' intervals are plotted for the full factorial design. If \code{term} is
#' supplied, factor-specific relative effects and confidence intervals are
#' plotted for the selected main effect or interaction. Term-specific plots
#' require that the model was fitted with \code{Factor.Information = TRUE}.
#'
#' @param x An object of class \code{"nparld_fit"}.
#' @param term Optional character vector specifying one or more model terms for
#'   which factor-specific relative effects and confidence intervals should be
#'   plotted. The requested terms require \code{Factor.Information = TRUE} in the
#'   original call to \code{nparLD()}. If \code{term = NULL}, the cell-level
#'   relative effects in \code{x$effects} are plotted.
#' @param xlab Optional x-axis label.
#' @param ylab Optional y-axis label. The default is \code{"Relative effect"}.
#' @param main Optional plot title.
#' @param legend.title Optional legend title.
#' @param ref.line Optional horizontal reference line. The default is \code{0.5}.
#' @param ref.lty Line type for the reference line.
#' @param ref.col Colour of the reference line.
#' @param ... Further arguments.
#'
#' @details
#' The default plot displays the estimated cell-level relative effects. If
#' \code{term} is supplied, the plot displays factor-specific relative effects
#' and confidence intervals for the selected main effects or interactions. This
#' requires that the model was fitted with \code{Factor.Information = TRUE}. 
#' The horizontal reference line at 0.5 indicates no tendency relative to the
#' reference distribution. Values above 0.5 indicate a tendency toward larger
#' responses, whereas values below 0.5 indicate a tendency toward smaller
#' responses.
#'
#' @examples
#' \donttest{
#' data(shoulder)
#'
#' fit <- nparLD(
#'   resp ~ group1 * group2 * time,
#'   data = shoulder,
#'   subject = "subject",
#'   hypothesis = "H0p",
#'   Factor.Information = TRUE
#' )
#'
#' ## Cell-level relative effects
#' plot(fit)
#'
#' ## Factor-specific relative effects and confidence intervals
#' plot(fit, term = "time")
#' plot(fit, term = "group1:time")
#' }
#'
#' @export
plot.nparld_fit <- function(x,
                            term = NULL,
                            xlab = NULL,
                            ylab = "Relative effect",
                            main = NULL,
                            legend.title = NULL,
                            ref.line = 0.5,
                            ref.lty = "dashed",
                            ref.col = "grey40",
                            ...) {

  if (is.null(term)) {
    return(.nparld_plot_cell_effects(
      x = x,
      xlab = xlab,
      ylab = ylab,
      main = main,
      legend.title = legend.title,
      ref.line = ref.line,
      ref.lty = ref.lty,
      ref.col = ref.col,
      ...
    ))
  }

  .nparld_plot_factor_information(
    x = x,
    term = term,
    reference = ref.line,
    ylab = ylab,
    xlab = xlab,
    main = main,
    ref.lty = ref.lty,
    ref.col = ref.col,
    ...
  )
}

.nparld_plot_cell_effects <- function(x,
                                      xlab = NULL,
                                      ylab = "Relative effect",
                                      main = NULL,
                                      legend.title = NULL,
                                      ref.line = 0.5,
                                      ref.lty = "dashed",
                                      ref.col = "grey40",
                                      ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }

  eff <- x$effects
  has_ci <- all(c("lower", "upper") %in% names(eff))

  non_factor_cols <- c(
    "n", "NAs",
    "Nsubj", "lambda", "Nobs", "Nmiss",
    "ScoreMean", "RTE", "std.error", "lower", "upper"
  )

  factor_cols <- setdiff(names(eff), non_factor_cols)
  nfac <- length(factor_cols)

  ## Preserve the order already stored in x$effects.
  ## This is important for numeric-looking factor levels such as 8, 10, 12, 14,
  ## which would otherwise be ordered lexicographically by ggplot2.
  for (v in factor_cols) {
    eff[[v]] <- factor(
      as.character(eff[[v]]),
      levels = unique(as.character(eff[[v]]))
    )
  }

  pd <- ggplot2::position_dodge(width = 0.3)

  p <- ggplot2::ggplot(eff, ggplot2::aes(y = RTE))

  # LD-F1
  if (nfac == 1) {
    p <- p +
      ggplot2::aes(x = .data[[factor_cols[1]]], group = 1) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_line()

    if (is.null(xlab)) xlab <- factor_cols[1]
  }

  # two factors
  if (nfac == 2) {
    p <- p +
      ggplot2::aes(
        x = .data[[factor_cols[2]]],
        color = .data[[factor_cols[1]]],
        group = .data[[factor_cols[1]]]
      ) +
      ggplot2::geom_point(size = 3, position = pd) +
      ggplot2::geom_line(position = pd)

    if (is.null(xlab)) xlab <- factor_cols[2]
    if (is.null(legend.title)) legend.title <- factor_cols[1]
  }

  # three or more factors
  if (nfac >= 3) {
    p <- p +
      ggplot2::aes(
        x = .data[[factor_cols[2]]],
        color = .data[[factor_cols[3]]],
        group = .data[[factor_cols[3]]]
      ) +
      ggplot2::geom_point(size = 3, position = pd) +
      ggplot2::geom_line(position = pd) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[[factor_cols[1]]])
      )

    if (is.null(xlab)) xlab <- factor_cols[2]
    if (is.null(legend.title)) legend.title <- factor_cols[3]
  }

  if (has_ci) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = lower, ymax = upper),
        width = 0.2,
        position = pd
      )
  }

  if (!is.null(ref.line)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = ref.line,
        linetype = ref.lty,
        colour = ref.col
      )
  }

  p +
    ggplot2::labs(
      title = main,
      x = xlab,
      y = ylab,
      color = legend.title
    ) +
    ggplot2::theme_bw()
}

.nparld_plot_factor_information <- function(x,
                                            term,
                                            reference = 0.5,
                                            ylab = "Relative effect",
                                            xlab = NULL,
                                            main = NULL,
                                            ref.lty = "dashed",
                                            ref.col = "grey40",
                                            ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for plotting.", call. = FALSE)
  }

  if (is.null(x$factor.info)) {
    stop(
      "No factor-specific information is available. ",
      "Refit the model with Factor.Information = TRUE.",
      call. = FALSE
    )
  }

  if (x$hypothesis != "H0p") {
    stop(
      "Factor-information confidence interval plots are available ",
      "only for hypothesis = 'H0p'.",
      call. = FALSE
    )
  }

  dat <- .nparld_collect_factor_information(x$factor.info, term)

estimate_col <- intersect(
  c("Rel.Effect", "RTE", "Estimate", "estimate"),
  names(dat)
)[1]

lower_col <- intersect(
  c("Lower", "lower", "lower.CL", "Lower.CL", "Lower.CI", "lower.CI"),
  names(dat)
)[1]

upper_col <- intersect(
  c("Upper", "upper", "upper.CL", "Upper.CL", "Upper.CI", "upper.CI"),
  names(dat)
)[1]
  if (is.na(estimate_col) || is.na(lower_col) || is.na(upper_col)) {
    stop(
      "Could not identify estimate and confidence interval columns in factor.info.",
      call. = FALSE
    )
  }

stat_cols <- c(
  "term", "Nsubj", "lambda", "Nobs", "Nmiss",
  "ScoreMean", "RTE", "Rel.Effect", "Estimate", "estimate",
  "std.error", "Std.Error", "SE",
  "lower", "Lower", "lower.CL", "Lower.CL", "Lower.CI", "lower.CI",
  "upper", "Upper", "upper.CL", "Upper.CL", "Upper.CI", "upper.CI"
)

if (!".label" %in% names(dat)) {
  label_cols <- setdiff(names(dat), stat_cols)

  if (length(label_cols) == 0L) {
    dat$.label <- seq_len(nrow(dat))
  } else {
    dat$.label <- apply(dat[label_cols], 1, paste, collapse = ":")
  }
}

  dat$.label <- factor(dat$.label, levels = unique(dat$.label))

  if (is.null(main)) {
  main <- paste("Estimated relative effects:", paste(term, collapse = ", "))
}

ggplot2::ggplot(
  dat,
  ggplot2::aes(
    x = .data[[".label"]],
    y = .data[[estimate_col]],
    ymin = .data[[lower_col]],
    ymax = .data[[upper_col]]
  )
) +
  ggplot2::geom_hline(
    yintercept = reference,
    linetype = ref.lty,
    colour = ref.col
  ) +
  ggplot2::geom_pointrange() +
  ggplot2::facet_wrap(ggplot2::vars(.data[["term"]]), scales = "free_x") +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = main
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme_bw()
}

.nparld_collect_factor_information <- function(factor.info, term) {

  if (is.list(factor.info) && !is.data.frame(factor.info)) {

    missing_terms <- setdiff(term, names(factor.info))

    if (length(missing_terms) > 0L) {
      stop(
        "The requested term is not available in factor.info: ",
        paste(missing_terms, collapse = ", "),
        call. = FALSE
      )
    }

dat <- do.call(rbind, lapply(term, function(tt) {
  z <- factor.info[[tt]]
  z <- as.data.frame(z)

  z$.label <- rownames(z)
  z$term <- tt

  z
}))

rownames(dat) <- NULL
return(dat)

    rownames(dat) <- NULL
    return(dat)
  }

  if (is.data.frame(factor.info)) {

    if (!"term" %in% names(factor.info)) {
      stop(
        "If factor.info is a data frame, it must contain a column named 'term'.",
        call. = FALSE
      )
    }

    dat <- factor.info[factor.info$term %in% term, , drop = FALSE]

    if (nrow(dat) == 0L) {
      stop(
        "The requested term is not available in factor.info: ",
        paste(term, collapse = ", "),
        call. = FALSE
      )
    }

    rownames(dat) <- NULL
    return(dat)
  }

  stop("Unsupported factor.info structure.", call. = FALSE)
}

#' Print nparLD test table
#'
#' @param x An object of class \code{"nparld_test_table"}.
#' @param digits Number of digits used for printing numerical results.
#' @param ... Further arguments.
#'
#' @export
print.nparld_test_table <- function(x, digits = 4, ...) {
  tab <- as.data.frame(x)
  class(tab) <- setdiff(class(tab), "nparld_test_table")
  .nparld_print_table(tab, digits = digits, ...)
  invisible(x)
}

##################################################################
# Plot simultaneous confidence intervals

#' Plot nparLD multiple contrast results
#'
#' Displays simultaneous confidence intervals for local contrasts from a
#' multiple contrast test procedure. The plot shows the contrast estimates
#' together with their simultaneous confidence limits and a vertical reference
#' line at zero. Intervals excluding zero are highlighted by default.
#'
#' @param x An object of class \code{"nparld_mctp"}.
#' @param xlab Label for the x-axis. The default is \code{"Contrast effect"}.
#' @param ylab Label for the y-axis. The default is an empty label.
#' @param main Optional plot title.
#' @param ref.line Optional vertical reference line. The default is \code{0}.
#' @param ref.lty Line type for the reference line.
#' @param ref.col Colour of the reference line.
#' @param pch Plotting character.
#' @param lwd Line width.
#' @param col Optional colors for confidence intervals and point estimates. If
#'   \code{NULL}, intervals excluding zero are shown in red and intervals
#'   including zero are shown in black.
#' @param ... Further graphical arguments.
#'
#' @details
#' Displays simultaneous confidence intervals for the local contrast results
#' returned by the multiple contrast test procedure. The vertical reference line
#' at zero indicates the null value for contrast effects.
#'
#' @examples
#' \donttest{
#' data(shoulder)
#'
#' fit <- nparLD(
#'   resp ~ group1 * group2 * time,
#'   data = shoulder,
#'   subject = "subject",
#'   hypothesis = "H0p",
#'   contrast = list("time", "Dunnett")
#' )
#'
#' plot(MCTP(fit))
#' }
#'
#' @export
plot.nparld_mctp <- function(x,
                             xlab = "Contrast effect",
                             ylab = "",
                             main = NULL,
                             ref.line = 0,
                             ref.lty = 2,
                             ref.col = "gray50",
                             pch = 19,
                             lwd = 2,
                             col = NULL,
                             ...) {

  if (is.null(x$Local.Results) || nrow(x$Local.Results) == 0L) {
    stop("No local contrast results available.", call. = FALSE)
  }

  res <- x$Local.Results

  if (!all(c("Estimate", "lower", "upper") %in% names(res))) {
    stop(
      "No simultaneous confidence intervals available for this object.",
      call. = FALSE
    )
  }

  est <- res$Estimate
  lower <- res$lower
  upper <- res$upper

  labs <- rownames(res)
  if (is.null(labs) || any(!nzchar(labs))) {
    labs <- paste("C", seq_len(nrow(res)))
  }

  k <- length(est)
  ypos <- rev(seq_len(k))

  if (is.null(col)) {
    sig <- !(lower <= ref.line & upper >= ref.line)
    col <- ifelse(sig, "red3", "black")
  }

  xr <- range(c(lower, upper, ref.line), na.rm = TRUE)

  if (is.null(main)) {
    main <- if (!is.null(x$Factor)) {
      paste("Simultaneous confidence intervals for", x$Factor)
    } else {
      "Simultaneous confidence intervals"
    }
  }

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar), add = TRUE)

  graphics::plot(
    NA,
    xlim = xr,
    ylim = c(0.5, k + 0.5),
    yaxt = "n",
    ylab = ylab,
    xlab = xlab,
    main = main,
    ...
  )

  graphics::axis(2, at = ypos, labels = labs, las = 1)
  graphics::abline(v = ref.line, lty = ref.lty, col = ref.col)

  graphics::segments(lower, ypos, upper, ypos, lwd = lwd, col = col)

  cap <- 0.12
  graphics::segments(lower, ypos - cap, lower, ypos + cap, lwd = lwd, col = col)
  graphics::segments(upper, ypos - cap, upper, ypos + cap, lwd = lwd, col = col)

  graphics::points(est, ypos, pch = pch, col = col)

  invisible(x)
}

#' Print nparLD permutation test results
#'
#' @param x An object of class \code{"nparld_permtest"}.
#' @param digits Number of digits used for printing numerical results.
#' @param ... Further arguments.
#'
#' @export
print.nparld_permtest <- function(x, digits = 4, ...) {
  cat("\nPermutation test\n")
  cat("-------------------------------------------------\n")

  if (!is.null(x$method)) {
    cat("Method: ", x$method, "\n", sep = "")
  }

  if (!is.null(x$table)) {
    .nparld_print_table(x$table, digits = digits)
  }

  invisible(x)
}