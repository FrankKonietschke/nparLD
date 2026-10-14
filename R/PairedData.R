

#*******************************************************************************
# Two time points
#*******************************************************************************

.can_use_perm_test <- function(design_obj) {
  
  # exactly one subplot factor
  if (length(design_obj$SP.names) != 1) return(FALSE)
  
  sp <- design_obj$SP.names
  
  # exactly 2 levels
  if (design_obj$n.levels[sp] != 2) return(FALSE)
  
  # no missing values
  if (any(is.na(design_obj$dat))) return(FALSE)
  
  TRUE
}

.extract_paired_samples <- function(design_obj) {
  
  dat <- design_obj$dat
  response <- all.vars(design_obj$formula)[1]
  sp <- design_obj$SP.names
  
  lev <- design_obj$names.levels[[sp]]
  
  # reshape into wide format (safe because design is balanced)
  wide <- reshape(
    dat,
    idvar = "subject",
    timevar = sp,
    direction = "wide"
  )
  
  x <- wide[[paste0(response, ".", lev[1])]]
  y <- wide[[paste0(response, ".", lev[2])]]
  
  list(x = x, y = y, lev = lev)
}

studentized_perm_test_paired <- function(
  x, y, B = 1000, alpha = 0.05,
  hypothesis = c("H0p", "H0F")
){
  
  n <- length(x)
  
compute_stat <- function(x, y, hypothesis) {
  
  n <- length(x)
  
  pooled <- c(x, y)
  ranks <- rank(pooled, ties.method = "average")
  
  R1 <- ranks[1:n]
  R2 <- ranks[(n+1):(2*n)]
  
  # --- estimator p-hat ---
  p_hat <- (mean(R2) - mean(R1)) / (2*n) + 0.5
  
  # --- variance depending on hypothesis ---
  
  if (hypothesis == "H0p") {
    
    # original Munzel placements
    R1_within <- rank(x, ties.method = "average")
    R2_within <- rank(y, ties.method = "average")
    
    Z <- (R2 - R2_within - R1 + R1_within) / n
    
    sigma_hat <- sd(Z)
    
  } else if (hypothesis == "H0F") {
    
    D <- (R2 - R1) / (2*n)   # scaling optional but recommended
    
    sigma_hat <- sd(D)
    
  }
  
  # safeguard
  if (sigma_hat == 0) sigma_hat <- 1/n
  
  M <- sqrt(n) * (p_hat - 0.5) / sigma_hat
  
  list(M = M, p_hat = p_hat, sigma = sigma_hat)
}
  
  # observed
  obs <- compute_stat(x, y, hypothesis)
  M_obs <- obs$M
se <- obs$sigma/sqrt(n)

# after computing obs, se, etc.

df <- n - 1

# t-based p-value
p_t <- 2 * (1 - pt(abs(M_obs), df = df))

# t-based CI (only meaningful for H0p)
t_quant <- qt(1 - alpha/2, df = df)

CI_t_lower <- obs$p_hat - t_quant * se
CI_t_upper <- obs$p_hat + t_quant * se
  
  # permutation distribution
  M_perm <- numeric(B)
  
  for (b in seq_len(B)) {
    swap <- rbinom(n, 1, 0.5) == 1
    
    x_perm <- x
    y_perm <- y
    
    x_perm[swap] <- y[swap]
    y_perm[swap] <- x[swap]
    
    M_perm[b] <- compute_stat(x_perm, y_perm, hypothesis)$M
  }
  
  # p-value
# permutation p-value with Monte Carlo correction
p_upper <- (1 + sum(M_perm >= M_obs)) / (B + 1)
p_lower <- (1 + sum(M_perm <= M_obs)) / (B + 1)

p_value <- min(1, 2 * min(p_upper, p_lower))
  
  # --- CI via permutation quantiles (Eq. 3.4) ---
  q_low  <- quantile(M_perm, probs = alpha/2, na.rm = TRUE)
  q_high <- quantile(M_perm, probs = 1 - alpha/2, na.rm = TRUE)
  
  se <- obs$sigma / sqrt(n)
  
  CI_lower <- obs$p_hat - q_high * se
  CI_upper <- obs$p_hat - q_low  * se
  
if (hypothesis == "H0F") {
  CI_lower <- NA
  CI_upper <- NA
}

list(
  statistic = M_obs,
  p.value = p_value,
  p.value.t = p_t,
  estimate = obs$p_hat,
  se = se,
  CI = c(lower = CI_lower, upper = CI_upper),
  CI.t = c(lower = CI_t_lower, upper = CI_t_upper),
  perm.dist = M_perm
)
}


.format_perm_output <- function(res, alpha = 0.05, hypothesis = "H0p") {
  
  if (hypothesis == "H0F") {
    
    out <- data.frame(
      Estimate = res$estimate,
      Std.Error = res$se,
      Statistic = res$statistic,
      p.t = res$p.value.t,
      p.perm = res$p.value
    )
    
    rownames(out) <- "Rank-based test"
    
  } else {
    
    out <- data.frame(
      Estimate = res$estimate,
      Std.Error = res$se,
      Lower.t = res$CI.t[1],
      Upper.t = res$CI.t[2],
      Lower.perm = res$CI[1],
      Upper.perm = res$CI[2],
      Statistic = res$statistic,
      p.t = res$p.value.t,
      p.perm = res$p.value
    )
    
    rownames(out) <- paste0("p (CI ", 100*(1-alpha), "%)")
  }
  
  out
}


