################################################################################
# Functions for general computations
################################################################################


.nparld_logit <- function(x){log(x/(1-x))}
.nparld_expit <- function(y){exp(y)/(1+exp(y))}

.nparld_limits <- function(p,V,alpha,N,CI.method){

  switch(CI.method,
                   normal={
  lower <- p - qnorm(1-alpha/2)/sqrt(N)*sqrt(c(diag(V)))
  upper <- p + qnorm(1-alpha/2)/sqrt(N)*sqrt(c(diag(V)))
               },
  logit = {Psi <- diag(1/(p*(1-p)))
  VLogit <- Psi%*%V%*%t(Psi)
  lower <- .nparld_expit(.nparld_logit(p)- qnorm(1-alpha/2)/sqrt(N)*sqrt(c(diag(VLogit))))
  upper <- .nparld_expit(.nparld_logit(p)+ qnorm(1-alpha/2)/sqrt(N)*sqrt(c(diag(VLogit))))}
  )

  res=cbind(lower=lower,upper=upper)
  res
}


.nparld_rankH <- function(A) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required for ginv().")
  }
  sum(diag(MASS::ginv(A) %*% A))
}

.nparld_wald <- function(M, H, V, N) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required for ginv().")
  }
  WTS <- N * t(H %*% M) %*% MASS::ginv(H %*% V %*% t(H)) %*% H %*% M
  dfWTS <- .nparld_rankH(H %*% V %*% t(H))
  pv.WTS <- 1 - pchisq(WTS, dfWTS)
  c(Statistic = as.numeric(WTS), df = as.numeric(dfWTS), `p-value` = as.numeric(pv.WTS))
}

.nparld_ats <- function(M, H, V, N) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required for ginv().")
  }
  C <- t(H) %*% MASS::ginv(H %*% t(H)) %*% H
  spur <- sum(diag(C %*% V))
  ATS <- N / spur * t(M) %*% C %*% M
  df_ATS1 <- spur^2 / sum(diag(C %*% V %*% C %*% V))
  pv.ATS <- 1 - pf(ATS, df_ATS1, 1e10)
  c(Statistic = as.numeric(ATS), df = as.numeric(df_ATS1), `p-value` = as.numeric(pv.ATS))
}

.nparld_run_tests <- function(theta_hat, Sigma, Hypotheses, N, output_names) {
  n.hypotheses <- length(Hypotheses)

  WTS <- matrix(0, n.hypotheses, 3)
  ATS <- matrix(0, n.hypotheses, 3)

  for (i in seq_len(n.hypotheses)) {
    WTS[i, ] <- .nparld_wald(theta_hat, Hypotheses[[i]], Sigma, N)
    ATS[i, ] <- .nparld_ats(theta_hat, Hypotheses[[i]], Sigma, N)
  }

  rownames(WTS) <- output_names
  rownames(ATS) <- output_names
  colnames(WTS) <- colnames(ATS) <- c("Statistic", "df", "p-value")

  list(WTS = WTS, ATS = ATS)
}





.nparld_run_factor.information <- function(theta_hat,
                                           Sigma,
                                           CIMatrices,
                                           Output.names,
                                           names.levels,
                                           alpha,
                                           N,
                                           CI.method,
                                           include.CI = FALSE) {

  n.factors <- length(CIMatrices)
  descriptives <- vector("list", n.factors)

  for (i in seq_len(n.factors)) {

    # effect estimates
    est <- as.vector(CIMatrices[[i]] %*% theta_hat)

    # covariance of estimates
    V <- CIMatrices[[i]] %*% Sigma %*% t(CIMatrices[[i]])

    se <- sqrt(diag(V))

    df <- data.frame(
      Rel.Effect = est,
      Std.Error  = se
    )

    # compute CIs if requested
    if (include.CI) {

      CI <- .nparld_limits(
        p = est,
        V = V,
        alpha = alpha,
        N = N,
        CI.method = CI.method
      )

      df$Lower <- CI[,1]
      df$Upper <- CI[,2]
    }

    # attach factor level names
    if(!is.null(names.levels[[Output.names[i]]])) {
      rownames(df) <- names.levels[[Output.names[i]]]
    }

    descriptives[[i]] <- df
  }

  names(descriptives) <- Output.names
  class(descriptives) <- "nparld_factorinfo"
descriptives
}


#*******************************************************************************
# Function for Hypotheses Matrices

HC <- function(fl, art=c("Hyp", "CI"),perm_names, names){
  # nf: number of factors involved
  # fl: vector of factorlevels, i.e. (a, b, c, ....) of size nf
  nf <- length(fl)
  switch(art, Hyp={

    P <- function(x){
      P <- diag(x) - matrix(1/x, ncol=x, nrow=x)
      return(P)}
  },
  CI = {

    P <- function(x){return(diag(x))}
  })
  # scaled one-vectors
  One <- function(x){
    I <- t(matrix(1/x, ncol=1, nrow=x))
    return(I)
  }

  if (nf>1){
    # number of hypotheses
    tmp <- 0
    for (i in 1:nf){
      tmp <- c(tmp, choose(nf, i))
      nh <- sum(tmp)
    }

    # calculate the permutation of the names
fac_names <- apply(perm_names, 1, function(x){
  paste(names[which(x == 1)], collapse=":")
})

    #nh: Number of Hypotheses
    # function for calculating the kronecker product of several matrices
    kp <- function(A){
      kp <- A[[1]]
      for (i in 2: length(A)){
        kp <- kp %x% A[[i]]
      }
      return(kp)
    }

    # scaled One-vectors for all factors
    A <- list()
    for (i in 1:nf){
      A[[i]] <- One(fl[i])
    }
    A_alt <- A

    # P-matrices for all factors
    B <- list()
    for (i in 1:nf){
      B[[i]] <- P(fl[i])
    }


    # calculate all combinations of elements of A and B (except all from A and all from B)
    n1 <- c(0, 1)
    liste <- vector("list", nf)
    for (i in 1:nf){
      liste[[i]] <- n1
    }
    G <- expand.grid(liste)
    G <- G[2:(dim(G)[[1]]-1),]        # 1 means A

    # list of all combinations of A and B
    C <- list()
    for (i in 1: dim(G)[1]){
      index <- which(G[i,]==1)
      for (j in 1:length(index)){
        A[[index[j]]] <- B[[index[j]]]
      }
      C[[i]] <- A
      A <- A_alt
    }


    # calculation of the hypotheses matrices (except for the nf-fold interaction)
    hypo <- vector("list", nh)
    for (i in 1:(nh-1)){
      C_tmp <- C[[i]]
      hypo[[i]] <- kp(C_tmp)
    }
    # nf-fold interaction
    hypo[[nh]] <- kp(B)
  }

  else {
    hypo <- list()
    hypo[[1]] <- P(fl)
    fac_names <- names
  }

  return(list(Matrix=hypo, Namen=fac_names))
}


.nparld_build_hypotheses <- function(formula, factor_cols, n.levels) {
  if (!exists("HC", mode = "function")) {
    stop("Function 'HC' not found. Make sure HC() is available in the package namespace.")
  }

  response_name <- all.vars(formula)[1]
  if (length(factor_cols) == 0) {
    stop("No factors found in the model (besides subject/response). Cannot build hypotheses.")
  }
  formula.new <- as.formula(paste(response_name, "~", paste(factor_cols, collapse = "*")))

  tf <- attr(terms(formula.new), "factors")
  if (nrow(tf) < 2) stop("Unexpected terms() structure; cannot extract factors for hypotheses.")

  fac_names <- rownames(tf)[-1]
  if (is.null(fac_names) || any(fac_names == "")) fac_names <- factor_cols

  perm_names <- t(tf[-1, , drop = FALSE])  # terms x factors (0/1)

  # Align perm_names columns to factor_cols if possible
  if (!is.null(colnames(perm_names)) && all(factor_cols %in% colnames(perm_names))) {
    perm_names <- perm_names[, factor_cols, drop = FALSE]
    fac_names <- factor_cols
  } else {
    fac_names <- factor_cols
  }

  Hyp <- HC(n.levels, "Hyp", perm_names, fac_names)
  CI  <- HC(n.levels, "CI",  perm_names, fac_names)

  list(
    Hypotheses   = Hyp$Matrix,
    CI.Matrices  = CI$Matrix,
    Output.names = Hyp$Namen,
    perm_names   = perm_names,
    fac_names    = fac_names
  )
}


# -----------------------------
# Helpers: theta (rank means) + effects table
.nparld_estimate_theta_rankmeans <- function(dat, formula, WP.names, SP.names,
                                             replicate = NULL,
                                             effect = c("unweighted", "weighted"),
                                             cell.weights = c("subjects", "observations")) {

  effect <- match.arg(effect)
  cell.weights <- match.arg(cell.weights)

  if ("PseudoRank" %in% names(dat)) {
    score_var <- "PseudoRank"
  } else {
    score_var <- "Ranks"
  }

  if (!(score_var %in% names(dat))) {
    dat[[score_var]] <- rank(dat[[1]], na.last = "keep")
  }

  if (!("subject" %in% names(dat))) {
    stop("Internal column 'subject' not found in data.", call. = FALSE)
  }

  factor_cols <- c(WP.names, SP.names)

  # canonical cell grid
  if (length(factor_cols) == 0L) {
    effects <- data.frame(dummy = 1)
    factor_cols <- character(0)
    cell_id <- rep("1", nrow(dat))
    cell_levels <- "1"
  } else {
    grid <- do.call(
      expand.grid,
      c(
        lapply(rev(factor_cols), function(v) levels(dat[[v]])),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    )
    names(grid) <- rev(factor_cols)
    grid <- grid[factor_cols]
    effects <- grid

    cell_id <- interaction(dat[, factor_cols, drop = FALSE], drop = TRUE, lex.order = TRUE)
    cell_levels <- interaction(grid[, factor_cols, drop = FALSE], drop = TRUE, lex.order = TRUE)
  }

  # number of subjects assigned to the cell
  n_subj <- tapply(dat$subject, cell_id, function(x) length(unique(x)))

  # number of subjects with at least one observed value in the cell
  lambda_cell <- tapply(seq_len(nrow(dat)), cell_id, function(idx) {
    df <- dat[idx, , drop = FALSE]
    by_subj <- split(df[[1]], df$subject)
    sum(vapply(by_subj, function(x) any(!is.na(x)), logical(1)))
  })

  # number of observed / missing responses per cell
  n_obs  <- tapply(!is.na(dat[[1]]), cell_id, sum)
  n_miss <- tapply(is.na(dat[[1]]),  cell_id, sum)

  effects$Nsubj <- 0L
  effects$lambda <- 0L
  effects$Nobs  <- 0L
  effects$Nmiss <- 0L

  pos_subj   <- match(names(n_subj), cell_levels)
  pos_lambda <- match(names(lambda_cell), cell_levels)
  pos_obs    <- match(names(n_obs), cell_levels)
  pos_miss   <- match(names(n_miss), cell_levels)

  if (length(pos_subj)   > 0) effects$Nsubj[pos_subj]     <- as.integer(n_subj)
  if (length(pos_lambda) > 0) effects$lambda[pos_lambda]  <- as.integer(lambda_cell)
  if (length(pos_obs)    > 0) effects$Nobs[pos_obs]       <- as.integer(n_obs)
  if (length(pos_miss)   > 0) effects$Nmiss[pos_miss]     <- as.integer(n_miss)

  # total number of observed responses
  Ntotal <- sum(effects$Nobs)

  # ---------------------------------------------------------------------------
  # cell score means
  # ---------------------------------------------------------------------------
  if (is.null(replicate) || cell.weights == "observations") {

    # no replicates OR observation-weighted within cell
    score_means <- tapply(dat[[score_var]], cell_id, function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0L) return(NA_real_)
      mean(x)
    })

  } else {

    # replicates present + subjects: equal subject weighting within cell
    if (!(replicate %in% names(dat))) {
      stop("Replicate variable '", replicate, "' not found in data.", call. = FALSE)
    }

    group_cols <- c("subject", factor_cols)

    subj_means <- aggregate(
      dat[[score_var]],
      by = dat[group_cols],
      FUN = function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0L) return(NA_real_)
        mean(x)
      }
    )
    colnames(subj_means)[ncol(subj_means)] <- "subj_score_mean"

    subj_cell_id <- if (length(factor_cols) == 0L) {
      rep("1", nrow(subj_means))
    } else {
      interaction(subj_means[, factor_cols, drop = FALSE], drop = TRUE, lex.order = TRUE)
    }

    score_means <- tapply(subj_means$subj_score_mean, subj_cell_id, function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0L) return(NA_real_)
      mean(x)
    })
  }

  effects$ScoreMean <- NA_real_
  pos_score <- match(names(score_means), cell_levels)
  if (length(pos_score) > 0) {
    effects$ScoreMean[pos_score] <- as.numeric(score_means)
  }

  effects$RTE <- 1 / Ntotal * (effects$ScoreMean - 1 / 2)

  if (length(factor_cols) > 0) {
    effects <- effects[do.call(order, effects[factor_cols]), , drop = FALSE]
  }

  list(
    dat = dat,
    effects = effects,
    theta_hat = effects$RTE,
    Ntotal = Ntotal
  )
}

# -----------------------------
# Helpers: covariance estimation (your block diagonal construction)
# -----------------------------
.nparld_estimate_sigma <- function(dat, WP.names, SP.names, N, Ntotal) {

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' is required for covariance estimation (bdiag).")
  }

  if (!("Ranks" %in% names(dat)) && !("PseudoRank" %in% names(dat))) {
    stop("Neither Ranks nor PseudoRank found in data.")
  }

  score_var <- if ("PseudoRank" %in% names(dat)) "PseudoRank" else "Ranks"

  cell_keys <- .nparld_cell_keys(dat, WP.names, SP.names)

  # subplot keys in canonical order
  if (length(SP.names) == 0) {
    sp_keys <- "1"
  } else {
    sp_grid <- do.call(
      expand.grid,
      c(
        lapply(rev(SP.names), function(v) levels(dat[[v]])),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    )
    names(sp_grid) <- rev(SP.names)
    sp_grid <- sp_grid[SP.names]
    sp_keys <- .nparld_make_keys(sp_grid, SP.names)
  }

  # whole-plot keys in canonical order
  if (length(WP.names) == 0) {
    wp_keys <- "1"
  } else {
    wp_grid <- do.call(
      expand.grid,
      c(
        lapply(rev(WP.names), function(v) levels(dat[[v]])),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    )
    names(wp_grid) <- rev(WP.names)
    wp_grid <- wp_grid[WP.names]
    wp_keys <- .nparld_make_keys(wp_grid, WP.names)
  }

  cov_from_units <- function(YY) {
    lambdas <- !is.na(YY)
    lambda_i <- colSums(lambdas)

    Cov_block <- matrix(0, ncol(YY), ncol(YY))
    Rbar <- colSums(YY, na.rm = TRUE) / lambda_i

    for (s in seq_len(ncol(YY))) {
      for (ss in seq_len(ncol(YY))) {
        if (s == ss) {
          Cov_block[s, ss] <- sum((YY[, s] - Rbar[s])^2, na.rm = TRUE) /
            (lambda_i[s] * (lambda_i[s] - 1))
        } else {
          Cov_block[s, ss] <- sum((YY[, s] - Rbar[s]) *
                                  (YY[, ss] - Rbar[ss]), na.rm = TRUE) /
            ((lambda_i[s] - 1) * (lambda_i[ss] - 1) +
               sum(lambdas[, s] * lambdas[, ss]) - 1)
        }
      }
    }

    Cov_block
  }

  Cov.blocks <- vector("list", length(wp_keys))

  for (g in seq_along(wp_keys)) {

    wp_key <- wp_keys[g]

    if (length(WP.names) == 0) {
      groupdata <- dat
    } else {
      dat_wp_key <- .nparld_make_keys(dat, WP.names)
      groupdata <- dat[dat_wp_key == wp_key, , drop = FALSE]
    }

    # IMPORTANT:
    # Independent units are SUBJECTS, not subject:replicate.
    groupdata$subject <- droplevels(groupdata$subject)
    subjdat <- split(groupdata, groupdata$subject)

    subj_vecs <- lapply(subjdat, function(df) {

      score <- df[[score_var]]

      if (length(SP.names) == 0) {
        subj_keys <- rep("1", nrow(df))
      } else {
        subj_keys <- .nparld_make_keys(df, SP.names)
      }

      # mean score per subject x subplot cell
      cell_means <- tapply(score, subj_keys, mean, na.rm = TRUE)

      yvec <- rep(NA_real_, length(sp_keys))
      names(yvec) <- sp_keys

      if (!is.null(cell_means)) {
        pos <- match(names(cell_means), sp_keys)
        ok <- !is.na(pos)

        if (any(ok)) {
          yvec[pos[ok]] <- 1 / Ntotal * (cell_means[ok] - 1 / 2)
        }
      }

      yvec
    })

    subj_vecs <- Filter(Negate(is.null), subj_vecs)

    if (length(subj_vecs) == 0) {
      stop("No valid observations found in this whole-plot block.")
    }

    YY_subj <- do.call(rbind, subj_vecs)
    YY_subj <- as.matrix(YY_subj)
    rownames(YY_subj) <- names(subj_vecs)
    colnames(YY_subj) <- sp_keys

    if (nrow(YY_subj) < 2) {
      stop("Too few subjects in whole-plot block.")
    }

    Cov.blocks[[g]] <- N * cov_from_units(YY_subj)

    rownames(Cov.blocks[[g]]) <- sp_keys
    colnames(Cov.blocks[[g]]) <- sp_keys
  }

  Sigma <- as.matrix(Matrix::bdiag(Cov.blocks))
  rownames(Sigma) <- cell_keys
  colnames(Sigma) <- cell_keys

  Sigma
}


.nparld_check_complete_within_schedule <- function(dat, SP.names, replicate = NULL) {
  if (length(SP.names) == 0) return(invisible(TRUE))

  if (!"subject" %in% names(dat)) {
    stop("Column 'subject' is required in 'dat'.", call. = FALSE)
  }

  # Missing design-factor values are not allowed here
  bad_sp <- SP.names[vapply(dat[SP.names], function(z) any(is.na(z)), logical(1))]
  if (length(bad_sp) > 0) {
    stop(
      "Missing values in subplot factor(s) are not allowed: ",
      paste(bad_sp, collapse = ", "),
      call. = FALSE
    )
  }

  # If there is no replicate variable, duplicates within subject are still suspicious
  if (is.null(replicate)) {
    within_key <- interaction(dat[, SP.names, drop = FALSE], drop = TRUE, lex.order = TRUE)
    dup <- ave(rep.int(1L, length(within_key)), dat$subject, within_key, FUN = sum)

    if (any(dup > 1L)) {
      bad <- dat[dup > 1L, c("subject", SP.names), drop = FALSE]
      bad <- unique(bad)
      stop(
        "Duplicate within-subject rows detected (same subplot combination occurs >1 time).\n",
        "Examples:\n",
        paste(utils::capture.output(print(utils::head(bad, 10))), collapse = "\n"),
        call. = FALSE
      )
    }
  }

  # Compare exact unique subplot schedule across subjects, ignoring replicate replication
  by_subj <- split(dat, dat$subject)

  schedule_signature <- function(df, cols) {
    sch <- unique(df[, cols, drop = FALSE])
    sch[] <- lapply(sch, as.character)
    sch <- sch[do.call(order, sch), , drop = FALSE]
    if (nrow(sch) == 0L) return("")
    paste(do.call(paste, c(sch, sep = "\r")), collapse = "\n")
  }

  sigs <- vapply(by_subj, schedule_signature, character(1), cols = SP.names)

  if (length(unique(sigs)) != 1L) {
    ref_subj <- names(sigs)[1]
    ref_sched <- unique(by_subj[[ref_subj]][, SP.names, drop = FALSE])
    ref_sched[] <- lapply(ref_sched, as.character)

    msg <- character(0)
    bad_subj <- names(sigs)[sigs != sigs[1]]

    for (s in utils::head(bad_subj, 5)) {
      cur_sched <- unique(by_subj[[s]][, SP.names, drop = FALSE])
      cur_sched[] <- lapply(cur_sched, as.character)

      ref_rows <- do.call(paste, c(ref_sched, sep = "\r"))
      cur_rows <- do.call(paste, c(cur_sched, sep = "\r"))

      missing <- setdiff(ref_rows, cur_rows)
      extra   <- setdiff(cur_rows, ref_rows)

      one <- paste0("Subject ", s, ":")
      if (length(missing) > 0) {
        one <- paste0(
          one, " missing ",
          paste(utils::head(missing, 10), collapse = ", "),
          if (length(missing) > 10) ", ..." else ""
        )
      }
      if (length(extra) > 0) {
        one <- paste0(
          one, if (length(missing) > 0) "; " else " ",
          "extra ",
          paste(utils::head(extra, 10), collapse = ", "),
          if (length(extra) > 10) ", ..." else ""
        )
      }
      msg <- c(msg, one)
    }

    stop(
      "Incomplete or inconsistent within-subject subplot schedule detected.\n",
      "Unequal replicate sizes are allowed, but subjects must share the same unique subplot combinations.\n",
      paste(msg, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.check_replicate_structure <- function(dat, WP.names, SP.names, replicate = NULL) {
  factor_cols <- c(WP.names, SP.names)
  key_cols <- c("subject", factor_cols)

  if (length(factor_cols) == 0L) return(invisible(TRUE))

  cell_id <- interaction(dat[, key_cols, drop = FALSE], drop = TRUE, lex.order = TRUE)
  n_per_cell <- table(cell_id)

  has_reps <- any(n_per_cell > 1L)

  if (has_reps && is.null(replicate)) {
    bad_ids <- names(n_per_cell)[n_per_cell > 1L]
    bad <- dat[cell_id %in% utils::head(bad_ids, 10), c(key_cols), drop = FALSE]
    bad <- unique(bad)

    stop(
      "Repeated rows within subject-by-cell combinations detected, but no 'replicate' variable was specified.\n",
      "If these are intended replicates, please provide replicate = <column_name>.\n",
      "Examples:\n",
      paste(utils::capture.output(print(utils::head(bad, 10))), collapse = "\n"),
      call. = FALSE
    )
  }

  if (!is.null(replicate)) {
    if (!(replicate %in% names(dat))) {
      stop("Replicate variable '", replicate, "' not found in data.", call. = FALSE)
    }

    rep_key_cols <- c(key_cols, replicate)
    rep_id <- interaction(dat[, rep_key_cols, drop = FALSE], drop = TRUE, lex.order = TRUE)
    n_per_rep <- table(rep_id)

    if (any(n_per_rep > 1L)) {
      bad_ids <- names(n_per_rep)[n_per_rep > 1L]
      bad <- dat[rep_id %in% utils::head(bad_ids, 10), rep_key_cols, drop = FALSE]
      bad <- unique(bad)

      stop(
        "Duplicate replicate IDs detected within subject-by-cell combinations.\n",
        "Examples:\n",
        paste(utils::capture.output(print(utils::head(bad, 10))), collapse = "\n"),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

##############################################################################################
# Functions for testing H0p
#############################################################################################

.nparld_cfun <- function(x, y) (x > y) + 0.5 * (x == y)

.nparld_build_Ghat <- function(split_obj, Fhat,
                               effect = c("unweighted", "weighted"),
                               cell.weights = c("subjects", "observations")) {

  effect <- match.arg(effect)
  cell.weights <- match.arg(cell.weights)

  S <- split_obj$struct
  wp_levels <- split_obj$WP_levels
  sp_levels <- split_obj$SP_levels

  F_list <- list()
  w_list <- numeric(0)

  idx <- 1L

  for (wp in wp_levels) {
    subj_list <- S[[wp]]

    for (sp in sp_levels) {
      F_list[[idx]] <- Fhat[[wp]][[sp]]

      vals_by_subj <- lapply(subj_list, function(one_subj) {
        v <- one_subj[[sp]]
        v[!is.na(v)]
      })

      m_ijk <- vapply(vals_by_subj, length, integer(1))
      lambda_ij <- sum(m_ijk > 0L)
      m_ij <- sum(m_ijk)

      if (cell.weights == "subjects") {
        w_list[idx] <- lambda_ij
      } else {
        w_list[idx] <- m_ij
      }

      idx <- idx + 1L
    }
  }

  function(xvec) {
    vapply(xvec, function(x) {
      vals <- vapply(F_list, function(F) F(x), numeric(1))

      if (effect == "unweighted") {
        mean(vals, na.rm = TRUE)
      } else {
        ok <- !is.na(vals) & (w_list > 0)
        if (!any(ok)) {
          NA_real_
        } else {
          sum(w_list[ok] * vals[ok]) / sum(w_list[ok])
        }
      }
    }, numeric(1))
  }
}

.nparld_compute_pseudoranksN <- function(dat, Ghat, N) {

  y <- dat[[1]]  # response is first column in model.frame

  pseudo_rank <- N * Ghat(y)+1/2

  dat$PseudoRank <- pseudo_rank
  dat
}


.nparld_estimate_p_hat_pseudo <- function(dat, WP.names, SP.names) {

  factor_cols <- c(WP.names, SP.names)

  by_subj_cell <- aggregate(
    PseudoRank ~ subject + .,
    data = dat[, c("PseudoRank", "subject", factor_cols), drop = FALSE],
    FUN = mean
  )

  effects <- aggregate(
    PseudoRank ~ .,
    data = by_subj_cell[, c("PseudoRank", factor_cols), drop = FALSE],
    FUN = mean
  )

  # explizite Zielordnung
  target_keys <- .nparld_cell_keys(dat, WP.names, SP.names)
  effect_keys <- .nparld_make_keys(effects, factor_cols)

  pos <- match(target_keys, effect_keys)
  effects <- effects[pos, , drop = FALSE]

  ad <- nrow(effects)
  p_hat <- effects$PseudoRank / ad
  names(p_hat) <- target_keys

  list(
    p_hat = as.numeric(p_hat),
    effects = effects
  )
}


.nparld_Fcell_eval <- function(cell_subj_reps, x, drop_subject = NULL) {
  # cell_subj_reps: named list(subject -> numeric reps)
  if (!is.null(drop_subject) && drop_subject %in% names(cell_subj_reps)) {
    cell_subj_reps <- cell_subj_reps[names(cell_subj_reps) != drop_subject]
  }
  vals <- vapply(cell_subj_reps, function(reps) {
    reps <- reps[!is.na(reps)]
    if (length(reps) == 0) return(NA_real_)
    mean(.nparld_cfun(x, reps))
  }, numeric(1))
  mean(vals, na.rm = TRUE)
}

.nparld_Ghat_loo_eval <- function(all_cells, x, drop_subject = NULL) {
  # all_cells: named list(cell_name -> named list(subject -> reps))
  mean(vapply(all_cells, function(cell_subj_reps) {
    .nparld_Fcell_eval(cell_subj_reps, x, drop_subject = drop_subject)
  }, numeric(1)))
}

.nparld_pairwise_p_hat <- function(split_obj, Fhat, cell.weights = c("subjects", "observations")) {

  cell.weights <- match.arg(cell.weights)

  S <- split_obj$struct
  wp_levels <- split_obj$WP_levels
  sp_levels <- split_obj$SP_levels

  a <- length(wp_levels)
  d <- length(sp_levels)
  ad <- a * d

  cell_names <- as.vector(t(outer(wp_levels, sp_levels, paste, sep = ":")))

  p_pair <- matrix(
    NA_real_,
    nrow = ad,
    ncol = ad,
    dimnames = list(cell_names, cell_names)
  )

  F_list <- vector("list", ad)
  idx <- 1L
  for (i in wp_levels) {
    for (s in sp_levels) {
      F_list[[idx]] <- Fhat[[i]][[s]]
      idx <- idx + 1L
    }
  }

  col_idx <- 1L

  for (h in wp_levels) {
    subj_list <- S[[h]]

    for (t in sp_levels) {

      vals_ht_by_subj <- lapply(subj_list, function(one_subj) {
        v <- one_subj[[t]]
        v[!is.na(v)]
      })

      m_htk <- vapply(vals_ht_by_subj, length, integer(1))
      lambda_ht <- sum(m_htk > 0L)
      m_ht <- sum(m_htk)

      if (cell.weights == "observations") {
        if (m_ht == 0L) {
          p_pair[, col_idx] <- NA_real_
        } else {
          vals_ht <- unlist(vals_ht_by_subj, use.names = FALSE)
          p_pair[, col_idx] <- vapply(
            F_list,
            function(F_is) mean(F_is(vals_ht)),
            numeric(1)
          )
        }
      } else {  # subjects
        if (lambda_ht == 0L) {
          p_pair[, col_idx] <- NA_real_
        } else {
          p_pair[, col_idx] <- vapply(
            F_list,
            function(F_is) {
              subj_means <- vapply(vals_ht_by_subj, function(v) {
                if (length(v) == 0L) return(NA_real_)
                mean(F_is(v))
              }, numeric(1))
              mean(subj_means, na.rm = TRUE)
            },
            numeric(1)
          )
        }
      }

      col_idx <- col_idx + 1L
    }
  }

  list(
    p_pair = p_pair,
    cell_names = cell_names,
    a = a,
    d = d,
    ad = ad,
    wp_levels = wp_levels,
    sp_levels = sp_levels
  )
}

#************************************************************************************************
# Covariance Matrix Estimation with two different weights

#*******************Helper for Null Variances in H0theta****************************************#

.nparld_effective_cell_sizes <- function(split_obj,
                                         cell.weights = c("subjects", "observations")) {
  cell.weights <- match.arg(cell.weights)

  S <- split_obj$struct
  wp_levels <- names(S)
  sp_levels <- split_obj$SP_levels

  n_eff <- matrix(
    0,
    nrow = length(wp_levels),
    ncol = length(sp_levels),
    dimnames = list(wp_levels, sp_levels)
  )

  for (h in wp_levels) {
    subj_list <- S[[h]]

    for (t in sp_levels) {
      n_eff[h, t] <- sum(vapply(subj_list, function(one_subj) {
        vals <- one_subj[[t]]
        vals <- vals[!is.na(vals)]

        if (cell.weights == "subjects") {
          as.numeric(length(vals) > 0L)
        } else {
          length(vals)
        }
      }, numeric(1)))
    }
  }

  ## Same ordering as row_of()/col_of(): group first, then condition
  as.numeric(t(n_eff))
}

.nparld_theta_variance_floor <- function(split_obj,
                                         cell.weights = c("subjects", "observations"),
                                         cell_names = NULL) {
  cell.weights <- match.arg(cell.weights)

  n_eff <- .nparld_effective_cell_sizes(split_obj, cell.weights = cell.weights)

  eps <- outer(n_eff, n_eff, function(ni, nj) {
    ifelse(
      ni > 0 & nj > 0,
      1 / (4 * ni^2 * nj^2),
      NA_real_
    )
  })

  diag(eps) <- NA_real_

  if (!is.null(cell_names)) {
    dimnames(eps) <- list(cell_names, cell_names)
  }

  eps
}

.nparld_regularize_theta_variance <- function(var_theta, eps_theta, tol = 0) {
  zero <- !is.finite(var_theta) | var_theta <= tol
  use  <- zero & is.finite(eps_theta)

  var_theta[use] <- eps_theta[use]
  var_theta
}

#***********************************************************************************************************#
# First is nu=2 = WEIGHTED replicates

.nparld_cov_H0p_ref_unweighted_observations <- function(split_obj, Fhat, C = NULL) {
  S <- split_obj$struct
  wp_levels <- names(S)
  sp_levels <- split_obj$SP_levels
  a <- length(wp_levels)
  d <- length(sp_levels)
  ad <- a * d

  # Pairwise p_hat*(is,ht)
  pp <- .nparld_pairwise_p_hat(split_obj, Fhat, cell.weights = "observations")
  p_pair <- pp$p_pair
  cell_names <- pp$cell_names

  eps_theta <- .nparld_theta_variance_floor(
  split_obj,
  cell.weights = "observations",
  cell_names = cell_names
)

  # helper: map (group, time) -> position in p_pair / Sigma
  col_of <- function(h, t) (match(h, wp_levels) - 1L) * d + match(t, sp_levels)
  row_of <- function(i, s) (match(i, wp_levels) - 1L) * d + match(s, sp_levels)

  # group sizes n_h (subjects per group)
  n_h <- vapply(wp_levels, function(h) length(S[[h]]), integer(1))
  N <- sum(n_h)

  # final covariance accumulator
  Vhat_sum <- matrix(0, ad, ad)

  # optional Satterthwaite storage for contrasts
  if (!is.null(C)) {
    q <- nrow(C)
    var_contr_h <- matrix(0, nrow = q, ncol = length(wp_levels))
    colnames(var_contr_h) <- wp_levels
    h_index <- setNames(seq_along(wp_levels), wp_levels)
  }

  for (h in wp_levels) {
    subj_list <- S[[h]]
    nh <- length(subj_list)
    if (nh < 2L) {
      stop("Need at least 2 subjects in group ", h, " for covariance.", call. = FALSE)
    }

    # subject-by-component matrix of Psi-beta
    Z <- matrix(0, nrow = nh, ncol = ad)

    # total number of observed observations in group h, time t
    m_ht <- setNames(
      vapply(sp_levels, function(t) {
        sum(vapply(subj_list, function(one_subj) {
          sum(!is.na(one_subj[[t]]))
        }, numeric(1)))
      }, numeric(1)),
      sp_levels
    )

    for (k_idx in seq_along(subj_list)) {
      one_subj <- subj_list[[k_idx]]

      # observed values for subject k in group h
      X_htk <- setNames(lapply(sp_levels, function(t) {
        v <- one_subj[[t]]
        v[!is.na(v)]
      }), sp_levels)

      # subject-specific counts m_htk
      m_htk <- setNames(vapply(X_htk, length, integer(1)), sp_levels)

      Psi  <- numeric(ad)
      beta <- numeric(ad)

      for (i in wp_levels) for (s in sp_levels) {
        is_idx <- row_of(i, s)

        if (h != i) {
          # ------------------------------------------------------------
          # h != i:
          #  Psi_{hk}(is) = -(n_h/ad) * sum_t sum_u [ 1/m_ht * F_is(X_htku) ]
          #  beta_{hk}(is)= -(n_h/ad) * sum_t [ m_htk/m_ht * p_hat*(is,ht) ]
          # ------------------------------------------------------------
          tmpPsi  <- 0
          tmpBeta <- 0

          for (t in sp_levels) {
            if (m_ht[[t]] <= 0) next

            vals <- X_htk[[t]]
            if (length(vals) > 0) {
              for (u in seq_along(vals)) {
                tmpPsi <- tmpPsi + (1 / m_ht[[t]]) * Fhat[[i]][[s]](vals[u])
              }
            }

            ht_idx <- col_of(h, t)
            tmpBeta <- tmpBeta + (m_htk[[t]] / m_ht[[t]]) * p_pair[is_idx, ht_idx]
          }

          Psi[is_idx]  <- -(nh / ad) * tmpPsi
          beta[is_idx] <- -(nh / ad) * tmpBeta

        } else {
          # ------------------------------------------------------------
          # h == i:
          #
          # Psi_{ik}(is) = (n_i/ad) * {
          #   sum_{j!=i} sum_t sum_u [ 1/m_is * F_jt(X_isku) ]
          # + sum_t sum_u [ 1/m_is * F_it(X_isku) ]
          # - sum_t sum_u [ 1/m_it * F_is(X_itku) ]
          # }
          #
          # beta_{ik}(is) = (n_i/ad) * {
          #   sum_{j!=i} sum_t [ m_isk/m_is * p_hat*(jt,is) ]
          # + sum_t         [ m_isk/m_is * p_hat*(it,is) ]
          # - sum_t         [ m_itk/m_it * p_hat*(is,it) ]
          # }
          # ------------------------------------------------------------
          tmpPsi1  <- 0
          tmpBeta1 <- 0

          # terms involving observations from cell (i,s):
          if (m_ht[[s]] > 0 && m_htk[[s]] > 0) {
            vals_is <- X_htk[[s]]

            for (j in wp_levels) {
              if (j == i) next

              for (t in sp_levels) {
                if (length(vals_is) > 0) {
                  for (u in seq_along(vals_is)) {
                    tmpPsi1 <- tmpPsi1 + (1 / m_ht[[s]]) * Fhat[[j]][[t]](vals_is[u])
                  }
                }

                jt_idx <- row_of(j, t)
                is_col <- col_of(i, s)
                tmpBeta1 <- tmpBeta1 + (m_htk[[s]] / m_ht[[s]]) * p_pair[jt_idx, is_col]
              }
            }
          }

          tmpPsi2  <- 0
          tmpBeta2 <- 0

          for (t in sp_levels) {
            # + sum_u [1/m_is * F_it(X_isku)]
            if (m_ht[[s]] > 0 && m_htk[[s]] > 0) {
              vals_is <- X_htk[[s]]
              if (length(vals_is) > 0) {
                for (u in seq_along(vals_is)) {
                  tmpPsi2 <- tmpPsi2 + (1 / m_ht[[s]]) * Fhat[[i]][[t]](vals_is[u])
                }
              }

              it_idx <- row_of(i, t)
              is_col <- col_of(i, s)
              tmpBeta2 <- tmpBeta2 + (m_htk[[s]] / m_ht[[s]]) * p_pair[it_idx, is_col]
            }

            # - sum_u [1/m_it * F_is(X_itku)]
            if (m_ht[[t]] > 0 && m_htk[[t]] > 0) {
              vals_it <- X_htk[[t]]
              if (length(vals_it) > 0) {
                for (u in seq_along(vals_it)) {
                  tmpPsi2 <- tmpPsi2 - (1 / m_ht[[t]]) * Fhat[[i]][[s]](vals_it[u])
                }
              }

              is_row <- row_of(i, s)
              it_col <- col_of(i, t)
              tmpBeta2 <- tmpBeta2 - (m_htk[[t]] / m_ht[[t]]) * p_pair[is_row, it_col]
            }
          }

          Psi[is_idx]  <- (nh / ad) * (tmpPsi1 + tmpPsi2)
          beta[is_idx] <- (nh / ad) * (tmpBeta1 + tmpBeta2)
        }
      }

      Z[k_idx, ] <- Psi - beta
    }

    # optional contrast-specific variances for Satterthwaite df
    if (!is.null(C)) {
      U <- Z %*% t(C)
      U[!is.finite(U)] <- 0
      var_h <- apply(U, 2, function(x) var(x, na.rm = TRUE))
      var_contr_h[, h_index[h]] <- var_h
    }

    Vh <- crossprod(scale(Z, center = colMeans(Z), scale = FALSE)) / (nh - 1)
    Vhat_sum <- Vhat_sum + (N / nh) * Vh
  }

  # optional Satterthwaite df
  dfs <- NULL
  if (!is.null(C)) {
    df <- numeric(q)
    for (j in seq_len(q)) {
      num <- (sum((N / n_h) * var_contr_h[j, ]))^2
      denom <- sum(((N / n_h)^2) * (var_contr_h[j, ]^2) / (n_h - 1))
      dfs[j] <- if (denom > 1e-12) num / denom else 10000
    }
    df <- round(max(4,min(dfs)))
  }

  dimnames(Vhat_sum) <- list(cell_names, cell_names)
list(
  Sigma = Vhat_sum,
  p_pair = p_pair,
  eps_theta = eps_theta,
  cell_names = cell_names,
  df_satt = df
)
}

#*************************************************************************************
# Next is nu = 1 = unweighted = DEFAULT

.nparld_cov_H0p_ref_unweighted_subjects <- function(split_obj, Fhat, C = NULL) {
  S <- split_obj$struct
  wp_levels <- names(S)
  sp_levels <- split_obj$SP_levels
  a <- length(wp_levels)
  d <- length(sp_levels)
  ad <- a * d

  # Pairwise p_hat*(is,ht) under subjects
  pp <- .nparld_pairwise_p_hat(split_obj, Fhat, cell.weights = "subjects")
  p_pair <- pp$p_pair
  cell_names <- pp$cell_names

  eps_theta <- .nparld_theta_variance_floor(
  split_obj,
  cell.weights = "subjects",
  cell_names = cell_names
)

  col_of <- function(h, t) (match(h, wp_levels) - 1L) * d + match(t, sp_levels)
  row_of <- function(i, s) (match(i, wp_levels) - 1L) * d + match(s, sp_levels)

  n_h <- vapply(wp_levels, function(h) length(S[[h]]), integer(1))
  N <- sum(n_h)

  Vhat_sum <- matrix(0, ad, ad)

  if (!is.null(C)) {
    q <- nrow(C)
    var_contr_h <- matrix(0, nrow = q, ncol = length(wp_levels))
    colnames(var_contr_h) <- wp_levels
    h_index <- setNames(seq_along(wp_levels), wp_levels)
  }

  for (h in wp_levels) {
    subj_list <- S[[h]]
    nh <- length(subj_list)
    if (nh < 2L) {
      stop("Need at least 2 subjects in group ", h, " for covariance.", call. = FALSE)
    }

    Z <- matrix(0, nrow = nh, ncol = ad)

    # subject-specific observed values and counts for group h
    X_hk <- lapply(subj_list, function(one_subj) {
      setNames(lapply(sp_levels, function(t) {
        v <- one_subj[[t]]
        v[!is.na(v)]
      }), sp_levels)
    })

    m_hk <- lapply(X_hk, function(xx) {
      setNames(vapply(xx, length, integer(1)), sp_levels)
    })

    # lambda_ht = number of subjects in group h with at least one obs in time t
    lambda_ht <- setNames(
      vapply(sp_levels, function(t) {
        sum(vapply(m_hk, function(mm) mm[[t]] > 0L, logical(1)))
      }, integer(1)),
      sp_levels
    )

    for (k_idx in seq_along(subj_list)) {
      X_htk <- X_hk[[k_idx]]
      m_htk <- m_hk[[k_idx]]

      Psi  <- numeric(ad)
      beta <- numeric(ad)

      for (i in wp_levels) for (s in sp_levels) {
        is_idx <- row_of(i, s)

        if (h != i) {
          tmpPsi  <- 0
          tmpBeta <- 0

          for (t in sp_levels) {
            if (lambda_ht[[t]] <= 0L || m_htk[[t]] <= 0L) next

            vals <- X_htk[[t]]
            w_htk <- 1 / (lambda_ht[[t]] * m_htk[[t]])

            for (u in seq_along(vals)) {
              tmpPsi <- tmpPsi + w_htk * Fhat[[i]][[s]](vals[u])
            }

            ht_idx <- col_of(h, t)
            tmpBeta <- tmpBeta + (1 / lambda_ht[[t]]) * p_pair[is_idx, ht_idx]
          }

          Psi[is_idx]  <- -(nh / ad) * tmpPsi
          beta[is_idx] <- -(nh / ad) * tmpBeta

        } else {
          tmpPsi1  <- 0
          tmpBeta1 <- 0

          # terms involving observations from cell (i,s)
          if (lambda_ht[[s]] > 0L && m_htk[[s]] > 0L) {
            vals_is <- X_htk[[s]]
            w_isk <- 1 / (lambda_ht[[s]] * m_htk[[s]])

            for (j in wp_levels) {
              if (j == i) next

              for (t in sp_levels) {
                for (u in seq_along(vals_is)) {
                  tmpPsi1 <- tmpPsi1 + w_isk * Fhat[[j]][[t]](vals_is[u])
                }

                jt_idx <- row_of(j, t)
                is_col <- col_of(i, s)
                tmpBeta1 <- tmpBeta1 + (1 / lambda_ht[[s]]) * p_pair[jt_idx, is_col]
              }
            }
          }

          tmpPsi2  <- 0
          tmpBeta2 <- 0

          for (t in sp_levels) {
            # + sum_u [1/(lambda_is m_isk) * F_it(X_isku)]
            if (lambda_ht[[s]] > 0L && m_htk[[s]] > 0L) {
              vals_is <- X_htk[[s]]
              w_isk <- 1 / (lambda_ht[[s]] * m_htk[[s]])

              for (u in seq_along(vals_is)) {
                tmpPsi2 <- tmpPsi2 + w_isk * Fhat[[i]][[t]](vals_is[u])
              }

              it_idx <- row_of(i, t)
              is_col <- col_of(i, s)
              tmpBeta2 <- tmpBeta2 + (1 / lambda_ht[[s]]) * p_pair[it_idx, is_col]
            }

            # - sum_u [1/(lambda_it m_itk) * F_is(X_itku)]
            if (lambda_ht[[t]] > 0L && m_htk[[t]] > 0L) {
              vals_it <- X_htk[[t]]
              w_itk <- 1 / (lambda_ht[[t]] * m_htk[[t]])

              for (u in seq_along(vals_it)) {
                tmpPsi2 <- tmpPsi2 - w_itk * Fhat[[i]][[s]](vals_it[u])
              }

              is_row <- row_of(i, s)
              it_col <- col_of(i, t)
              tmpBeta2 <- tmpBeta2 - (1 / lambda_ht[[t]]) * p_pair[is_row, it_col]
            }
          }

          Psi[is_idx]  <- (nh / ad) * (tmpPsi1 + tmpPsi2)
          beta[is_idx] <- (nh / ad) * (tmpBeta1 + tmpBeta2)
        }
      }

      Z[k_idx, ] <- Psi - beta
    }

    if (!is.null(C)) {
      U <- Z %*% t(C)
      U[!is.finite(U)] <- 0
      var_h <- apply(U, 2, function(x) var(x, na.rm = TRUE))
      var_contr_h[, h_index[h]] <- var_h
    }

    Vh <- crossprod(scale(Z, center = colMeans(Z), scale = FALSE)) / (nh - 1)
    Vhat_sum <- Vhat_sum + (N / nh) * Vh
  }

  df <- NULL
  if (!is.null(C)) {
    dfs <- numeric(q)
    for (j in seq_len(q)) {
      num <- (sum((N / n_h) * var_contr_h[j, ]))^2
      denom <- sum(((N / n_h)^2) * (var_contr_h[j, ]^2) / (n_h - 1))
      dfs[j] <- if (denom > 1e-12) num / denom else 10000
    }
    df <- round(max(4, min(dfs)))
  }

  dimnames(Vhat_sum) <- list(cell_names, cell_names)

list(
  Sigma = Vhat_sum,
  p_pair = p_pair,
  eps_theta = eps_theta,
  cell_names = cell_names,
  df_satt = df
)
}

#*******************************************************************************************



.nparld_cfun <- function(x, y) {
  (x > y) + 0.5 * (x == y)
}

.nparld_split_by_wp_subject_sp <- function(dat, WP.names, SP.names) {

  dat$..WP <- .nparld_make_keys(dat, WP.names)
  dat$..SP <- .nparld_make_keys(dat, SP.names)

  wp_keys <- if(length(WP.names)==0) "1" else {
    wp_grid <- expand.grid(lapply(rev(WP.names), function(v) levels(dat[[v]])),
                           KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    names(wp_grid) <- rev(WP.names)
    wp_grid <- wp_grid[WP.names]
    do.call(paste, c(lapply(wp_grid, as.character), sep=":"))
  }

  sp_keys <- if(length(SP.names)==0) "1" else {
    sp_grid <- expand.grid(lapply(rev(SP.names), function(v) levels(dat[[v]])),
                           KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    names(sp_grid) <- rev(SP.names)
    sp_grid <- sp_grid[SP.names]
    do.call(paste, c(lapply(sp_grid, as.character), sep=":"))
  }

  by_wp <- split(dat, factor(dat$..WP, levels = wp_keys))

  out <- lapply(by_wp, function(dw) {
    dw$subject <- droplevels(dw$subject)
    by_subj <- split(dw, dw$subject)

    lapply(by_subj, function(ds) {
      by_sp <- split(ds[[1]], factor(ds$..SP, levels = sp_keys))
      by_sp[sapply(by_sp, is.null)] <- list(numeric(0))
      by_sp
    })
  })

  names(out) <- wp_keys

  list(
    struct = out,
    WP_levels = wp_keys,
    SP_levels = sp_keys
  )
}


.nparld_build_Fhat <- function(split_obj, cell.weights = c("subjects", "observations")) {
  cell.weights <- match.arg(cell.weights)

  S <- split_obj$struct
  wp_levels <- split_obj$WP_levels
  sp_levels <- split_obj$SP_levels

  Fhat <- lapply(wp_levels, function(wp) {
    subj_list <- S[[wp]]

    lapply(sp_levels, function(sp) {

      # observations per subject in this cell
      reps_by_subj <- lapply(subj_list, function(one_subj) {
        v <- one_subj[[sp]]
        v[!is.na(v)]
      })

      # pooled observations in this cell
      reps_all <- unlist(reps_by_subj, use.names = FALSE)

      function(xvec) {
        vapply(xvec, function(x) {

          if (cell.weights == "subjects") {
            # subject-unweighted
            vals <- vapply(reps_by_subj, function(reps) {
              if (length(reps) == 0L) return(NA_real_)
              mean(.nparld_cfun(x, reps))
            }, numeric(1))

            if (all(is.na(vals))) {
              NA_real_
            } else {
              mean(vals, na.rm = TRUE)
            }
          } else {
            # observations: observation-weighted
            if (length(reps_all) == 0L) {
              NA_real_
            } else {
              mean(.nparld_cfun(x, reps_all))
            }
          }

        }, numeric(1))
      }
    })
  })

  names(Fhat) <- wp_levels
  for (wp in wp_levels) names(Fhat[[wp]]) <- sp_levels

  Fhat
}


#*******************************************************************************
# Multiple Contrast Tests
#*******************************************************************************

.nparld_build_contrast_matrix <- function(contrast, H){

  k <- nrow(H)

  # default: identity contrasts
  if (length(contrast) == 1) {
    CC <- diag(k)
    rownames(CC) <- rownames(diag(k))
  }

  if (length(contrast) == 2) {

    if (is.character(contrast[[2]])) {

      if (!requireNamespace("multcomp", quietly = TRUE))
        stop("Package 'multcomp' required for built-in contrasts")

      CC <- multcomp::contrMat(rep(1, k), type = contrast[[2]])
    }

    if (is.numeric(contrast[[2]])) {
      if (length(contrast[[2]]) != k)
        stop("Contrast length does not match number of factor levels.")
      CC <- matrix(contrast[[2]], ncol = k)
      rownames(CC) <- "Custom"
    }

    if (is.matrix(contrast[[2]])) {
      CC <- contrast[[2]]
      if (is.null(rownames(CC))) {
        rownames(CC) <- paste0("C", seq_len(nrow(CC)))
      }
    }
  }

  # center contrasts
  if (any(rowSums(CC) != 0)) {
    rows <- which(rowSums(CC) != 0)
    CC[rows, ] <- CC[rows, ] - rowMeans(CC)[rows]
  }

  CC
}

.nparld_regularize_contrast_cov <- function(Vuse, Cfull, eps_theta) {
  vars <- diag(Vuse)

  eps_contrast <- apply(Cfull, 1, function(cc) {
    involved <- which(abs(cc) > 0)

    if (length(involved) < 2L) {
      return(NA_real_)
    }

    eps_sub <- eps_theta[involved, involved, drop = FALSE]
    eps_sub <- eps_sub[is.finite(eps_sub)]

    if (length(eps_sub) == 0L) {
      NA_real_
    } else {
      min(eps_sub)
    }
  })

  vars.reg <- .nparld_regularize_theta_variance(
    var_theta = vars,
    eps_theta = eps_contrast
  )

  diag(Vuse) <- vars.reg
  Vuse
}

.nparld_MCTP <- function(theta_hat, Sigma, H, C, N, alpha,
                         sci.method = c("fisher", "multi.t"),
                         hypothesis,
                         df_satt = NULL,
                         eps_theta = NULL) {

  if (!requireNamespace("mvtnorm", quietly = TRUE))
    stop("Package 'mvtnorm' required.")
  eps_theta = NULL
  sci.method <- match.arg(sci.method)

  pd.main <- H %*% theta_hat
  V.main  <- H %*% Sigma %*% t(H)

  CH.pd <- as.vector(C %*% pd.main)
  CH.V  <- C %*% V.main %*% t(C)



Cfull <- C %*% H

if (!is.null(eps_theta)) {
  CH.V <- .nparld_regularize_contrast_cov(
    Vuse = CH.V,
    Cfull = Cfull,
    eps_theta = eps_theta
  )
}

  # degrees of freedom
  df <- if (!is.null(df_satt)) min(df_satt) else max(4, N - 1)

  # ------------------------------------------------------------------
  # H0F: no simultaneous confidence intervals, no Fisher transformation
  # ------------------------------------------------------------------
  if (hypothesis == "H0F") {

    Est <- CH.pd
    Vuse <- CH.V
    SE <- sqrt(diag(Vuse))
    Tvec <- Est / (SE / sqrt(N))
    R <- cov2cor(Vuse)

    pv <- sapply(seq_along(Tvec), function(i) {
      1 - mvtnorm::pmvt(
        lower = -abs(Tvec[i]),
        upper =  abs(Tvec[i]),
        corr = R,
        delta = rep(0, nrow(R)),
        df = df
      )[1]
    })

    crit <- mvtnorm::qmvt(
      1 - alpha,
      delta = rep(0, nrow(R)),
      corr = R,
      tail = "both",
      df = df
    )$quantile

    res <- data.frame(
      Estimate  = Est,
      Std.Error = SE,
      Statistic = Tvec,
      p.value   = pv,
      df        = df
    )

    return(list(
      Contrast.Matrix = C %*% H,
      Local.Results   = res,
      Global.Result   = data.frame(
        Statistic = max(abs(Tvec)),
        p.value   = min(pv)
      ),
      DF = df,
      Quantile = crit,
      sci.method = NA_character_
    ))
  }

  # ------------------------------------------------------------------
  # H0p: simultaneous confidence intervals are computed
  # ------------------------------------------------------------------

  if (sci.method == "multi.t") {

    Est <- CH.pd
    Vuse <- CH.V
    SE <- sqrt(diag(Vuse))
    Tvec <- Est / (SE / sqrt(N))
    R <- cov2cor(Vuse)

    pv <- sapply(seq_along(Tvec), function(i) {
      1 - mvtnorm::pmvt(
        lower = -abs(Tvec[i]),
        upper =  abs(Tvec[i]),
        corr = R,
        delta = rep(0, nrow(R)),
        df = df
      )[1]
    })

    crit <- mvtnorm::qmvt(
      1 - alpha,
      delta = rep(0, nrow(R)),
      corr = R,
      tail = "both",
      df = df
    )$quantile

    lower <- Est - crit * SE / sqrt(N)
    upper <- Est + crit * SE / sqrt(N)

    res <- data.frame(
      Estimate  = Est,
      Std.Error = SE,
      lower     = lower,
      upper     = upper,
      Statistic = Tvec,
      p.value   = pv,
      df        = df
    )

    return(list(
      Contrast.Matrix = C %*% H,
      Local.Results   = res,
      Global.Result   = data.frame(
        Statistic = max(abs(Tvec)),
        p.value   = min(pv)
      ),
      DF = df,
      Quantile = crit,
      sci.method = sci.method
    ))
  }

  # ------------------------------------------------------------------
  # H0p + Fisher transformation + delta method
  # ------------------------------------------------------------------

  eps <- 1e-10
  if (any(abs(CH.pd) >= 1 - eps)) {
    stop("Fisher transformation requires all contrast estimates to lie strictly between -1 and 1.",
         call. = FALSE)
  }

  g <- function(x) 0.5 * log((1 + x) / (1 - x))
  ginv <- function(y) tanh(y)
  gp <- function(x) 1 / (1 - x^2)

  Est.raw <- CH.pd
  Est <- g(Est.raw)

  D <- diag(gp(Est.raw), nrow = length(Est.raw), ncol = length(Est.raw))
  Vuse <- D %*% CH.V %*% D

  SE <- sqrt(diag(Vuse))
  Tvec <- Est / (SE / sqrt(N))
  R <- cov2cor(Vuse)

  pv <- sapply(seq_along(Tvec), function(i) {
    1 - mvtnorm::pmvt(
      lower = -abs(Tvec[i]),
      upper =  abs(Tvec[i]),
      corr = R,
      delta = rep(0, nrow(R)),
      df = df
    )[1]
  })

  crit <- mvtnorm::qmvt(
    1 - alpha,
    delta = rep(0, nrow(R)),
    corr = R,
    tail = "both",
    df = df
  )$quantile

  lower.z <- Est - crit * SE / sqrt(N)
  upper.z <- Est + crit * SE / sqrt(N)

  lower <- ginv(lower.z)
  upper <- ginv(upper.z)

  res <- data.frame(
    Estimate  = Est.raw,
    Std.Error = SE,
    lower     = lower,
    upper     = upper,
    Statistic = Tvec,
    p.value   = pv,
    df        = df
  )

  list(
    Contrast.Matrix = C %*% H,
    Local.Results   = res,
    Global.Result   = data.frame(
      Statistic = max(abs(Tvec)),
      p.value   = min(pv)
    ),
    DF = df,
    Quantile = crit,
    sci.method = sci.method
  )
}

.nparld_run_contrasts <- function(theta_hat,
                                  Sigma,
                                  H,
                                  C,
                                  N,
                                  alpha,
                                  sci.method,
                                  hypothesis,
                                  df_satt = NULL,
                                  eps_theta = NULL,
                                  factor_name = NULL) {

res <- .nparld_MCTP(
  theta_hat = theta_hat,
  Sigma = Sigma,
  H = H,
  C = C,
  N = N,
  alpha = alpha,
  sci.method = sci.method,
  hypothesis = hypothesis,
  df_satt = df_satt,
  eps_theta = eps_theta
)

  # build readable contrast labels
  contrast_labels <- rownames(C)

  if (is.null(contrast_labels)) {
    contrast_labels <- paste("Contrast", seq_len(nrow(C)))
  }

  rownames(res$Local.Results) <- contrast_labels
  rownames(res$Contrast.Matrix) <- contrast_labels

  if (!is.null(factor_name)) {
    res$Factor <- factor_name
  }

  class(res) <- "nparld_mctp"
  res
}

