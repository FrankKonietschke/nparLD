#' Extract Wald-type test results
#'
#' Extracts the Wald-type statistic table from a fitted nparLD model.
#'
#' @param object An object of class \code{"nparld_fit"}.
#' @param ... Further arguments.
#'
#' @return A matrix or data frame containing Wald-type test statistics.
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
#' @return A matrix or data frame containing ANOVA-type test statistics.
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
  object$effects
}

#' Extract covariance matrix
#'
#' Extracts the estimated covariance matrix of the relative effect estimator
#' from a fitted nparLD model.
#'
#' @param object An object of class \code{"nparld_fit"}.
#' @param ... Further arguments.
#'
#' @return A covariance matrix.
#' @export
covariance <- function(object, ...) {
  UseMethod("covariance")
}

#' @export
covariance.nparld_fit <- function(object, ...) {
  if (is.null(object$covariance)) {
    stop(
      "No covariance matrix is available. ",
      "Refit with covariance = TRUE.",
      call. = FALSE
    )
  }

  object$covariance
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
#' @return A permutation test object.
#' @export
permTest <- function(object, ...) {
  UseMethod("permTest")
}

#' @param table Logical. If \code{TRUE}, return only the permutation test table.
#' @rdname permTest
#' @export
permTest.nparld_fit <- function(object, table = FALSE, ...) {
  if (is.null(object$perm) || !isTRUE(object$perm$available)) {
    stop("No permutation test results are available.", call. = FALSE)
  }

if (isTRUE(table)) {
  out <- as.data.frame(object$perm$table)
  class(out) <- c("nparld_test_table", class(out))
  return(out)
}

  out <- object$perm
  class(out) <- c("nparld_permtest", class(out))
  out
}


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