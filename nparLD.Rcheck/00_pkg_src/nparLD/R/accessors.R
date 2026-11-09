#' Extract Wald-type test results
#'
#' Extracts the Wald-type statistic table from a fitted nparLD model.
#'
#' @param object An object of class \code{"nparld_fit"}.
#' @param ... Further arguments.
#'
#' @return A data frame containing Wald-type test statistics.
#' @export
WTS <- function(object, ...) {
  UseMethod("WTS")
}

#' @export
WTS.nparld_fit <- function(object, ...) {
  out <- as.data.frame(object$WTS)
  class(out) <- c("nparld_test_table", class(out))
  out
}


#' Extract ANOVA-type test results
#'
#' Extracts the ANOVA-type statistic table from a fitted nparLD model.
#'
#' @param object An object of class \code{"nparld_fit"}.
#' @param ... Further arguments.
#'
#' @return A data frame containing ANOVA-type test statistics.
#' @export
ATS <- function(object, ...) {
  UseMethod("ATS")
}

#' @export
ATS.nparld_fit <- function(object, ...) {
  out <- as.data.frame(object$ATS)
  class(out) <- c("nparld_test_table", class(out))
  out
}

#' Extract multiple contrast test results
#'
#' Extracts the multiple contrast test procedure from a fitted nparLD model.
#'
#' @param object An object of class \code{"nparld_fit"}.
#' @param ... Further arguments.
#'
#' @return An object of class \code{"nparld_mctp"}.
#' @export
MCTP <- function(object, ...) {
  UseMethod("MCTP")
}

#' @export
MCTP.nparld_fit <- function(object, ...) {
  if (is.null(object$MCTP)) {
    stop("No multiple contrast test results are available.", call. = FALSE)
  }
  object$MCTP
}


#' Extract factor-specific information
#'
#' Extracts factor-specific relative effects from a fitted nparLD model.
#'
#' @param object An object of class \code{"nparld_fit"}.
#' @param ... Further arguments.
#'
#' @return An object of class \code{"nparld_factorinfo"}.
#' @export
factorInfo <- function(object, ...) {
  UseMethod("factorInfo")
}

#' @export
factorInfo.nparld_fit <- function(object, ...) {
  if (is.null(object$factor.info)) {
    stop(
      "No factor-specific information is available. ",
      "Refit with Factor.Information = TRUE.",
      call. = FALSE
    )
  }
  object$factor.info
}


#' Extract estimated relative effects
#'
#' Extracts the estimated relative effects table from a fitted nparLD model.
#'
#' @param object An object of class \code{"nparld_fit"}.
#' @param ... Further arguments.
#'
#' @return A data frame containing estimated relative effects.
#'
#' @importFrom stats effects
#' @export
effects.nparld_fit <- function(object, ...) {

  out <- as.data.frame(object$effects)

  class(out) <- c(
    "nparld_effects",
    class(out)
  )

  out
}

#' @export
print.nparld_effects <- function(x, digits = 4, ...) {

  tab <- as.data.frame(x)

  num_cols <- vapply(
    tab,
    is.numeric,
    logical(1)
  )

  tab[num_cols] <- lapply(
    tab[num_cols],
    round,
    digits = digits
  )

  print(
    tab,
    quote = FALSE,
    ...
  )

  invisible(x)
}


#' Extract covariance matrix
#'
#' Extracts the estimated asymptotic covariance matrix of
#' \eqn{\sqrt{N}(\hat{\theta} - \theta)} from a fitted nparLD model.
#'
#' @details
#' The returned matrix estimates the covariance matrix on the
#' \eqn{\sqrt{N}} asymptotic scale. Consequently, the covariance
#' matrix of \eqn{\hat{\theta}} itself is obtained approximately as
#' \code{covariance(object) / N}, where \code{N} is the number of
#' independent subjects.
#'
#' @param object An object of class \code{"nparld_fit"}.
#' @param ... Further arguments.
#'
#' @return The estimated asymptotic covariance matrix of
#' \eqn{\sqrt{N}(\hat{\theta} - \theta)}.
#'
#' @export
covariance <- function(object, ...) {
  UseMethod("covariance")
}

#' @rdname covariance
#' @export
covariance.nparld_fit <- function(object, ...) {
  if (is.null(object$covariance.info)) {
    stop(
      "No covariance matrix is available. ",
      "Refit with covariance = TRUE.",
      call. = FALSE
    )
  }

  object$covariance.info
}



#' Extract contrast matrix
#'
#' Extracts the contrast matrix from a multiple contrast test procedure.
#'
#' @param object An object of class \code{"nparld_mctp"} or \code{"nparld_fit"}.
#' @param ... Further arguments.
#'
#' @return A contrast matrix.
#' @export
contrastMatrix <- function(object, ...) {
  UseMethod("contrastMatrix")
}

#' @export
contrastMatrix.nparld_mctp <- function(object, ...) {
  object$Contrast.Matrix
}

#' @export
contrastMatrix.nparld_fit <- function(object, ...) {
  contrastMatrix(MCTP(object), ...)
}

#' Extract permutation test results
#'
#' Extracts permutation test results from a fitted nparLD model.
#'
#' @param object An object of class \code{"nparld_fit"}.
#' @param ... Further arguments passed to methods.
#'
#' @return An object of class \code{"nparld_permtest"} containing:
#' \describe{
#'   \item{\code{available}}{Logical indicating whether the permutation test was available.}
#'   \item{\code{method}}{Character string describing the permutation procedure.}
#'   \item{\code{table}}{A compact table with the effect estimate, standard error,
#'   test statistic, and p-values; for \code{H0p}, confidence intervals are also included.}
#'   \item{\code{raw}}{The underlying unformatted permutation-test results, including
#'   the permutation distribution.}
#'   \item{\code{levels}}{The two levels of the repeated-measures factor used in the test.}
#' }
#'
#' @export
permTest <- function(object, ...) {
  UseMethod("permTest")
}

#' @rdname permTest
#' @export
permTest.nparld_fit <- function(object, ...) {
  if (is.null(object$perm) || !isTRUE(object$perm$available)) {
    stop("No permutation test results are available.", call. = FALSE)
  }

  out <- object$perm
  class(out) <- c("nparld_permtest", class(out))
  out
}

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