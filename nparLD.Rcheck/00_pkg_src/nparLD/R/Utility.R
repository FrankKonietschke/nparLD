#*******************************************************************************
# Output Functions
#*******************************************************************************

utils::globalVariables(c(".data", "RTE", "Lower", "Upper"))

.nparld_print_table <- function(tab, digits = 4, ...) {

  tab <- as.data.frame(tab)
  cutoff <- 10^(-digits)
  for (nm in names(tab)) {
   if (nm %in% c("p.value", "p.t", "p.perm")) {
      p <- tab[[nm]]
      tab[[nm]] <- ifelse(
        is.na(p),
        NA_character_,
        ifelse(
          p < cutoff,
          paste0("<", format(cutoff, scientific = TRUE)),
          formatC(
            p,
            format = "f",
            digits = digits
          )
        )
      )
    } else if (is.numeric(tab[[nm]])) {
      tab[[nm]] <- round(
        tab[[nm]],
        digits = digits
      )
    }
  }
  print(
    tab,
    quote = FALSE,
    ...
  )
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
#' @export
print.nparld_covarianceinfo <- function(x, digits = 4, ...) {
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

  for (i in seq_along(x)) {

    if (i > 1L) cat("\n")
    cat("Term: ", names(x)[i], "\n", sep = "")

    tab <- as.data.frame(x[[i]])

    num_cols <- vapply(tab, is.numeric, logical(1))

    tab[num_cols] <- lapply(
      tab[num_cols],
      round,
      digits = digits
    )

    print(tab, quote = FALSE)
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
    alpha = object$.internal$alpha,

    effects = object$effects,
    factor.info = object$factor.info,

    WTS = object$WTS,
    ATS = object$ATS,

    MCTP = object$MCTP,

    covariance.available = !is.null(object$covariance.info),

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
    alpha <- x$alpha

if (!is.null(alpha) &&
    length(alpha) == 1L &&
    is.finite(alpha)) {

    cat(
      "Confidence level: ",
      format(
        100 * (1 - x$alpha),
        trim = TRUE
      ),
      "%\n",
      sep = ""
    )
}
  }

  if (!is.null(x$effects)) {
    cat("\nEstimated relative effects (RTE)\n")
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

  if (!is.null(x$Contrast.Type)) {
    cat(
      "Contrast type: ",
      x$Contrast.Type,
      "\n",
      sep = ""
    )
  }

if (!is.null(x$sci.method) &&
    length(x$sci.method) == 1L &&
    !is.na(x$sci.method)) {

  cat(
    "SCI method: ",
    x$sci.method,
    "\n",
    sep = ""
  )

  if (!is.null(x$alpha) &&
      length(x$alpha) == 1L &&
      is.finite(x$alpha)) {

    cat(
      "Confidence level: ",
      format(
        100 * (1 - x$alpha),
        trim = TRUE
      ),
      "%\n",
      sep = ""
    )
  }
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
      cat(
        "Use `print(MCTP(x), show.matrix = TRUE)` to display it,\n",
        "or `contrastMatrix(x)` to extract it.\n",
        sep = ""
      )
    }
  }

  if (identical(x$sci.method, "fisher")) {
    cat(
      "\nNote: For sci.method = \"fisher\", test statistics are on the\n",
      "Fisher-transformed scale; estimates, SEs, and SCIs are reported\n",
      "on the original scale.\n",
      sep = ""
    )
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
  has_ci <- all(c("Lower", "Upper") %in% names(eff))

  subtitle <- NULL

if (is.null(main)) {

  if (has_ci) {

    alpha <- if (!is.null(x$.internal$alpha)) {
      x$.internal$alpha
    } else {
      0.05
    }

    conf_level <- 100 * (1 - alpha)

    main <- paste0(
      format(conf_level, trim = TRUE),
      "% confidence intervals"
    )

    subtitle <- "Cell-level relative effects"

  } else {

    main <- "Estimated relative effects"
    subtitle <- "Cell-level effects"
  }
}

  non_factor_cols <- c(
    "n", "NAs",
    "Nsubj", "lambda", "Nobs", "Nmiss",
    "ScoreMean", "RTE", "Std.Error", "Lower", "Upper"
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
        ggplot2::aes(ymin = Lower, ymax = Upper),
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
  subtitle = subtitle,
  x = xlab,
  y = ylab,
  color = legend.title
)+
    ggplot2::theme_bw(base_size = 16) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 18),
    plot.subtitle = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 16),
    axis.text = ggplot2::element_text(size = 15),
    legend.title = ggplot2::element_text(size = 15),
    legend.text = ggplot2::element_text(size = 14)
  )
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
    stop(
      "Package 'ggplot2' required for plotting.",
      call. = FALSE
    )
  }

  if (is.null(x$factor.info)) {
    stop(
      "No factor-specific information is available. ",
      "Refit the model with Factor.Information = TRUE.",
      call. = FALSE
    )
  }

  dat <- .nparld_collect_factor_information(
    x$factor.info,
    term
  )

  # ------------------------------------------------------------
  # Identify estimate column
  # ------------------------------------------------------------

  estimate_col <- intersect(
    c(
      "Rel.Effect",
      "RTE",
      "Estimate",
      "estimate"
    ),
    names(dat)
  )[1L]

  if (
    length(estimate_col) == 0L ||
    is.na(estimate_col)
  ) {
    stop(
      "Could not identify the effect-estimate column in factor.info.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------
  # Identify confidence interval columns, if available
  # ------------------------------------------------------------

  lower_col <- intersect(
    c(
      "Lower",
      "lower",
      "lower.CL",
      "Lower.CL",
      "Lower.CI",
      "lower.CI"
    ),
    names(dat)
  )[1L]

  upper_col <- intersect(
    c(
      "Upper",
      "upper",
      "upper.CL",
      "Upper.CL",
      "Upper.CI",
      "upper.CI"
    ),
    names(dat)
  )[1L]

  has_ci <-
    length(lower_col) > 0L &&
    length(upper_col) > 0L &&
    !is.na(lower_col) &&
    !is.na(upper_col)

  show_ci <-
    identical(x$hypothesis, "H0p") &&
    has_ci


  # ------------------------------------------------------------
  # Construct labels
  # ------------------------------------------------------------

  stat_cols <- c(
    "term",
    "Nsubj",
    "lambda",
    "Nobs",
    "Nmiss",
    "ScoreMean",
    "RTE",
    "Rel.Effect",
    "Estimate",
    "estimate",
    "std.error",
    "Std.Error",
    "SE",
    "lower",
    "Lower",
    "lower.CL",
    "Lower.CL",
    "Lower.CI",
    "lower.CI",
    "upper",
    "Upper",
    "upper.CL",
    "Upper.CL",
    "Upper.CI",
    "upper.CI"
  )

  if (!".label" %in% names(dat)) {

    label_cols <- setdiff(
      names(dat),
      stat_cols
    )

    if (length(label_cols) == 0L) {

      dat$.label <- seq_len(
        nrow(dat)
      )

    } else {

      dat$.label <- apply(
        dat[label_cols],
        1L,
        paste,
        collapse = ":"
      )
    }
  }

  dat$.label <- factor(
    dat$.label,
    levels = unique(dat$.label)
  )


  # ------------------------------------------------------------
  # Default title
  # ------------------------------------------------------------

subtitle <- NULL

if (is.null(main)) {

  if (show_ci) {

    alpha <- if (!is.null(x$.internal$alpha)) {
      x$.internal$alpha
    } else {
      0.05
    }

    conf_level <- 100 * (1 - alpha)

    main <- paste0(
      format(conf_level, trim = TRUE),
      "% confidence intervals"
    )

  } else {

    main <- "Estimated relative effects"
  }

  subtitle <- paste0(
    "Factor: ",
    paste(term, collapse = ", ")
  )
}


  # ------------------------------------------------------------
  # Base plot
  # ------------------------------------------------------------

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data[[".label"]],
      y = .data[[estimate_col]]
    )
  )


  # ------------------------------------------------------------
  # Reference line
  # ------------------------------------------------------------

  if (!is.null(reference)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = reference,
        linetype = ref.lty,
        colour = ref.col
      )
  }


  # ------------------------------------------------------------
  # Estimates
  #
  # H0p: estimates + confidence intervals
  # H0F: estimates only
  # ------------------------------------------------------------

  if (show_ci) {

    p <- p +
      ggplot2::geom_pointrange(
        ggplot2::aes(
          ymin = .data[[lower_col]],
          ymax = .data[[upper_col]]
        )
      )

  } else {

    p <- p +
      ggplot2::geom_point(
        size = 3
      )
  }


  # ------------------------------------------------------------
  # Final formatting
  # ------------------------------------------------------------

  p +
    ggplot2::facet_wrap(
      ggplot2::vars(
        .data[["term"]]
      ),
      scales = "free_x"
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = main,
      subtitle = subtitle
    ) +
    ggplot2::coord_cartesian(
      ylim = c(0, 1)
    ) +
ggplot2::theme_bw(base_size = 16) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 18),
    plot.subtitle = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 16),
    axis.text = ggplot2::element_text(size = 15),
    legend.title = ggplot2::element_text(size = 15),
    legend.text = ggplot2::element_text(size = 14)
  )

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
#' @param point.size Numeric. Point size for the estimated contrast effects.
#' @param line.width Numeric. Line width for the simultaneous confidence intervals.
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
                             ref.lty = "dashed",
                             ref.col = "grey50",
                             point.size = 3,
                             line.width = 0.8,
                             ...) {

  if (is.null(x$Local.Results) || nrow(x$Local.Results) == 0L) {
    stop(
      "No local contrast results available.",
      call. = FALSE
    )
  }

  res <- x$Local.Results

  if (!all(c("Estimate", "Lower", "Upper") %in% names(res))) {
    stop(
      "No simultaneous confidence intervals available for this object.",
      call. = FALSE
    )
  }

  labs <- rownames(res)

  if (is.null(labs) || any(!nzchar(labs))) {
    labs <- paste0(
      "C",
      seq_len(nrow(res))
    )
  }

  dat <- data.frame(
    contrast = labs,
    Estimate = res$Estimate,
    Lower = res$Lower,
    Upper = res$Upper,
    stringsAsFactors = FALSE
  )

  # Preserve the order shown in Local.Results
  dat$contrast <- factor(
    dat$contrast,
    levels = rev(dat$contrast)
  )

  # CI excludes the null value
  dat$significant <- !(
    dat$Lower <= ref.line &
      dat$Upper >= ref.line
  )

alpha <- x$alpha

if (is.null(alpha) ||
    length(alpha) != 1L ||
    !is.finite(alpha)) {
  alpha <- 0.05
}

conf_level <- 100 * (1 - alpha)

  subtitle <- NULL
  if (is.null(main)) {
    main <- paste0(
      format(conf_level, trim = TRUE),
      "% simultaneous confidence intervals"
    )

    if (!is.null(x$Factor) &&
        !is.null(x$Contrast.Type)) {

      subtitle <- paste0("Factor: ",
        x$Factor,
        " (",
        x$Contrast.Type,
        ")"
      )

    } else if (!is.null(x$Factor)) {

      subtitle <- paste0(
  "Factor: ",
  x$Factor
)
    }
  }

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data$Estimate,
      y = .data$contrast
    )
  ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = main,
      subtitle = subtitle
    ) +
    ggplot2::geom_vline(
      xintercept = ref.line,
      linetype = ref.lty,
      colour = ref.col
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = .data$Lower,
        xmax = .data$Upper,
        colour = .data$significant
      ),
      width = 0.18,
      linewidth = line.width,
      orientation = "y"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        colour = .data$significant
      ),
      size = point.size
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "FALSE" = "black",
        "TRUE" = "red3"
      ),
      guide = "none"
    ) +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 18
      ),
      plot.subtitle = ggplot2::element_text(
        size = 16
      ),
      axis.title = ggplot2::element_text(
        size = 16
      ),
      axis.text = ggplot2::element_text(
        size = 16
      )
    )
}



#' Print nparLD permutation test results
#'
#' @param x An object of class \code{"nparld_permtest"}.
#' @param digits Number of digits used for printing numerical results.
#' @param ... Further arguments.
#'
#' @export
print.nparld_permtest <- function(x, digits = 4, ...) {
  if (!is.null(x$table)) {
    .nparld_print_table(
      x$table,
      digits = digits
    )
  }

  invisible(x)
}

#' Plot the permutation distribution
#'
#' Displays the studentized permutation distribution together with
#' the observed test statistic.
#'
#' @param x An object returned by \code{permTest()}.
#' @param breaks Histogram breaks passed to \code{hist()}.
#' @param ... Additional graphical arguments passed to \code{hist()}.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @export
plot.nparld_permtest <- function(
  x,
  breaks = "FD",
  ...
) {

  if (is.null(x$raw) ||
      is.null(x$raw$perm.dist) ||
      length(x$raw$perm.dist) == 0L) {
    stop(
      "No permutation distribution is available.",
      call. = FALSE
    )
  }

  perm <- x$raw$perm.dist
  stat <- x$raw$statistic

  if (is.null(stat) ||
      length(stat) != 1L ||
      !is.finite(stat)) {
    stop(
      "No observed test statistic is available.",
      call. = FALSE
    )
  }

  lim <- range(
  c(
    perm,
    -abs(stat),
    abs(stat)
  ),
  finite = TRUE
)

graphics::hist(
  perm,
  breaks = breaks,
  xlim = lim,
  main = "Studentized permutation distribution",
  xlab = "Studentized test statistic",
    ...
  )

  graphics::abline(
    v = c(-abs(stat), abs(stat)),
    lty = 2,
    lwd = 2
  )

  graphics::legend(
    "topright",
    legend = "Observed |T|",
    lty = 2,
    lwd = 2,
    bty = "n"
  )

  invisible(NULL)
}