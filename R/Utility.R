#*******************************************************************************
# Output Functions
#*******************************************************************************
utils::globalVariables(c(".data", "RTE", "lower", "upper"))
#' Print nparLD fit
#'
#' @param x An object of class \code{nparld_fit}.
#' @param ... Further arguments.
#' @export
print.nparld_fit <- function(x, ...) {

  cat("\n ---- Nonparametric Longitudinal Analysis ---- \n")
  cat("-------------------------------------------------\n")

  if (!is.null(x$Design)) {
  cat(" #Longitudinal Design:", x$Design, "\n", sep="")
  if(!is.null(x$wholeplots)){cat(" #Whole-Plot-Factors:", x$wholeplots, "\n", sep = "")}
  cat(" #Sub-Plot-Factors:", x$subplots, "\n", sep = "") }
  if (!is.null(x$N.info)) cat(" #Number of subjects:", x$N.info, "\n", sep = "")
  cat(" #Hypotheses:", x$text.hypotheses, "\n", sep = "") 
  cat(" #Type of ranking:", x$text.ranks, "\n", sep = "") 
  if(x$hypothesis=="H0p") cat(" #CI Method:", x$CI.method, "\n", sep="")
 

  cat("\n Estimated relative effects \n")
  cat("-------------------------------------------------\n")
  print(x$effects)
  if(!is.null(x$factor.info)) print(x$factor.info)

  cat("\nGobal Test Results: \n")
   cat("-------------------------------------------------\n")
  cat("\nWald-Type Statistic (WTS)\n")
  print(round(x$WTS, 4))

  cat("\nANOVA-Type Statistic (ATS)\n")
  print(round(x$ATS, 4))

 
  if(!is.null(x$MCTP)) print(x$MCTP)

  if(!is.null(x$covariance.info)) print(x$covariance.info)

  if (!is.null(x$perm) && isTRUE(x$perm$available)) {
    
    cat("\nPermutation Test (Studentized):\n")
    cat("-------------------------------------------------\n")
    print(x$perm$table)
  }


  invisible(x)
}

#' Print nparLD covariance matrix
#'
#' @param x An object of class \code{nparld_covarianceinfo}.
#' @param digits Number of digits.
#' @param ... Further arguments.
#' @export

print.nparld_covarianceinfo <- function(x, digits = 4, ...) {

  cat("\nEstimated covariance matrix\n")
  cat("-------------------------------------------------\n")

  print(round(unclass(x), digits = digits))

  invisible(x)
}

#' Print nparLD factor information
#'
#' @param x An object of class \code{nparld_factorinfo}.
#' @param digits Number of digits.
#' @param ... Further arguments.
#' @export

print.nparld_factorinfo <- function(x, digits = 4, ...) {

  cat("\nFactor Information\n")
  cat("------------------\n")

  for(i in seq_along(x)){

    cat("\nFactor:", names(x)[i], "\n\n")

    tab <- x[[i]]

    tab[] <- lapply(tab, function(z) round(z, digits))

    print(tab)
  }

  invisible(x)
}


#' Summarize an nparLD fit
#'
#' @param object An object of class \code{nparld_fit}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return An object of class \code{summary.nparld_fit}.
#' @export
summary.nparld_fit <- function(object, ...) {

  res <- list(
    Design = object$Design,
    N = object$N.info,
    effects = object$effects,
    WTS = object$WTS,
    ATS = object$ATS
  )
  
  if(!is.null(object$MCTP)){

    res$MCTP <- object$MCTP}
  # --- NEW: add permutation results ---
  if (!is.null(object$perm) && isTRUE(object$perm$available)) {
    res$Permutation <- object$perm
  }

  class(res) <- "summary.nparld_fit"
  res
}

#' Print Summary nparLD Fit
#'
#' @param x An object of class \code{nparld_fit}.
#' @param digits Number of digits.
#' @param ... Further arguments.
#' @export
print.summary.nparld_fit <- function(x, digits = 4, ...) {

  cat("\nDesign:\n")
  cat(x$Design, "\n")

  cat("\nNumber of subjects:", x$N, "\n")

  cat("\nEffects:\n")
  print(x$effects)

  cat("\nWTS:\n")
  print(round(x$WTS, digits))

  cat("\nATS:\n")
  print(round(x$ATS, digits))

  if(!is.null(x$MCTP)){

    cat("MCTP: \n")
    print(x$MCTP)
  }

  # --- NEW: permutation output ---
  if (!is.null(x$Permutation) && isTRUE(x$Permutation$available)) {
    
    cat("\nPermutation Test (Studentized):\n")
    cat("----------------------------------\n")
    print(round(x$Permutation$table, digits))
  }

  invisible(x)
}

#' Print nparLD multiple contrast results
#'
#' @param x An object of class \code{"nparld_mctp"}.
#' @param digits Number of digits.
#' @param matrix Logical. If \code{TRUE}, print the contrast matrix used in the
#'   multiple contrast procedure.
#' @param ... Further arguments.
#' @export
print.nparld_mctp <- function(x, digits = 4, matrix = FALSE, ...) {

  cat("\n Multiple Contrast Test Procedure \n")
  cat("-------------------------------------------------\n")

  if (!is.null(x$Factor)) {
    cat(" Factor or interaction:", x$Factor, "\n", sep = "")
  }

  if (!is.null(x$sci.method) && !is.na(x$sci.method)) {
    cat(" SCI method:", x$sci.method, "\n", sep = "")
  }

  if (!is.null(x$Global.Result)) {
    cat("\n Global result \n")
    cat("-------------------------------------------------\n")
    print(round(x$Global.Result, digits = digits))
  }

  if (!is.null(x$Local.Results)) {
    cat("\n Local contrast results \n")
    cat("-------------------------------------------------\n")
    print(round(x$Local.Results, digits = digits))
  }

  if (!is.null(x$Contrast.Matrix)) {
    if (isTRUE(matrix)) {
      cat("\n Contrast matrix \n")
      cat("-------------------------------------------------\n")
      print(round(x$Contrast.Matrix, digits = digits))
    } else {
      cat("\n Contrast matrix: available in component `Contrast.Matrix`.\n")
      cat(" To print it, use `print(x, matrix = TRUE)` on the MCTP object.\n")
    }
  }

  invisible(x)
}

#' Plot relative effects from an nparLD fit
#'
#' @param x An object of class \code{nparld_fit}.
#' @param ... Further arguments passed to the plotting function.
#' @param main Optional main title.
#' @param xlab Optional label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param legend.title Optional legend title.
#' @param ref.line Reference line shown in the plot. Use \code{NULL} to suppress it.
#' @param ref.lty Line type of the reference line.
#' @param ref.col Color of the reference line.
#'
#' @return A \code{ggplot} object.
#' @export
plot.nparld_fit <- function(x, ...,
                            main = NULL,
                            xlab = NULL,
                            ylab = "Relative Treatment Effect",
                            legend.title = NULL,
                            ref.line = 0.5,
                            ref.lty = 2,
                            ref.col = "grey50") {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for plotting.")

  eff <- x$effects
  has_ci <- all(c("lower", "upper") %in% names(eff))

  non_factor_cols <- c(
    "n", "NAs",
    "Nsubj", "lambda", "Nobs", "Nmiss",
    "ScoreMean", "RTE", "std.error", "lower", "upper"
  )

  factor_cols <- setdiff(names(eff), non_factor_cols)
  nfac <- length(factor_cols)

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

##################################################################
#Plot the SCI

#' Plot multiple contrast test results
#'
#' @param x An object of class \code{nparld_mctp}.
#' @param xlab Label for the x-axis.
#' @param main Plot title.
#' @param pch Plotting character.
#' @param lwd Line width.
#' @param col Optional colors.
#' @param ... Further graphical arguments.
#' @export
plot.nparld_mctp <- function(x,
                             xlab = "Contrast effect",
                             main = NULL,
                             pch = 19,
                             lwd = 2,
                             col = NULL,
                             ...) {

  if (is.null(x$Local.Results) || nrow(x$Local.Results) == 0) {
    stop("No local contrast results available.", call. = FALSE)
  }

  res <- x$Local.Results

  if (!all(c("Estimate", "lower", "upper") %in% names(res))) {
    stop("No simultaneous confidence intervals available for this object.",
         call. = FALSE)
  }

  est <- res$Estimate
  lower <- res$lower
  upper <- res$upper

  labs <- rownames(res)
  if (is.null(labs)) {
    labs <- paste("C", seq_len(nrow(res)))
  }

  k <- length(est)
  ypos <- rev(seq_len(k))

  if (is.null(col)) {
    sig <- !(lower <= 0 & upper >= 0)
    col <- ifelse(sig, "red3", "black")
  }

  xr <- range(c(lower, upper, 0), na.rm = TRUE)

  if (is.null(main)) {
    main <- if (!is.null(x$Factor)) {
      paste("Simultaneous confidence intervals for", x$Factor)
    } else {
      "Simultaneous confidence intervals"
    }
  }

  plot(NA,
       xlim = xr,
       ylim = c(0.5, k + 0.5),
       yaxt = "n",
       ylab = "",
       xlab = xlab,
       main = main,
       ...)

  axis(2, at = ypos, labels = labs, las = 1)
  abline(v = 0, lty = 2, col = "gray50")

  segments(lower, ypos, upper, ypos, lwd = lwd, col = col)

  cap <- 0.12
  segments(lower, ypos - cap, lower, ypos + cap, lwd = lwd, col = col)
  segments(upper, ypos - cap, upper, ypos + cap, lwd = lwd, col = col)

  points(est, ypos, pch = pch, col = col)

  invisible(x)
}