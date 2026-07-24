## extremes_functions.R --------------------------------------------------------
## All model functions for the streamflow extremes pipeline. Sourced by
## run_pipeline.R. Stage labels follow the project plan:
##   S3  stationary marginal POT (+ declustered robustness variant)
##   S9a non-stationary threshold, linear-in-time special case (Lee & Lee 2020
##       form of Jonathan et al. 2014, Section 3.2)
##   S4  semiparametric transform to Gumbel margins (HT04 eq. (1.3)-(1.4))
##   S5  HT04 conditional dependence model, Gaussian estimation (HT04 5.2)
##   S7  conditional simulation (HT04 Section 4.3)
##   S8  water-year block bootstrap replicate

## ==================================================================== S3: GPD

## Negative log-likelihood of GP(sigma, xi) for excesses y > 0.
## Parameterization par = c(log sigma, xi).
gpd_nll <- function(par, y) {
  sig <- exp(par[1]); xi <- par[2]
  if (abs(xi) < 1e-8) return(length(y) * par[1] + sum(y) / sig)
  z <- 1 + xi * y / sig
  if (any(z <= 0)) return(1e10)
  length(y) * par[1] + (1 + 1 / xi) * sum(log(z))
}

## Per-observation contributions (needed for the Smith sandwich).
gpd_nll_obs <- function(par, y) {
  sig <- exp(par[1]); xi <- par[2]
  if (abs(xi) < 1e-8) return(par[1] + y / sig)
  z <- 1 + xi * y / sig
  z[z <= 0] <- NA
  par[1] + (1 + 1 / xi) * log(z)
}

fit_gpd <- function(y) {
  p0 <- c(log(mean(y)), 0.1)
  o1 <- optim(p0, gpd_nll, y = y, method = "Nelder-Mead",
              control = list(maxit = 2000))
  o2 <- tryCatch(optim(o1$par, gpd_nll, y = y, method = "BFGS",
                       control = list(maxit = 500)),
                 error = function(e) NULL)
  o  <- if (!is.null(o2) && o2$value <= o1$value) o2 else o1
  H  <- tryCatch(optimHess(o$par, gpd_nll, y = y), error = function(e) NULL)
  se <- c(NA_real_, NA_real_)
  if (!is.null(H)) {
    Hi <- tryCatch(solve(H), error = function(e) NULL)
    if (!is.null(Hi)) se <- sqrt(pmax(diag(Hi), 0))
  }
  sig <- exp(o$par[1])
  list(par = o$par, sigma = sig, xi = o$par[2],
       se_naive = c(sigma = sig * se[1], xi = se[2]),  # delta method for sigma
       hessian = H, nll = o$value, conv = o$convergence, n = length(y))
}

## Smith (1990) sandwich covariance, as used by Lee & Lee (2020, Appendix):
## point estimates from the working independence likelihood; standard errors
## from H^{-1} V H^{-1} with V built from the empirical covariance of score
## sums grouped into (approximately independent) water years.
smith_sandwich_gpd <- function(fit, y, grp) {
  if (is.null(fit$hessian)) return(c(sigma = NA_real_, xi = NA_real_))
  par <- fit$par
  h   <- 1e-5 * (abs(par) + 1)
  G   <- matrix(NA_real_, length(y), 2)
  for (k in 1:2) {
    pp <- par; pm <- par
    pp[k] <- pp[k] + h[k]; pm[k] <- pm[k] - h[k]
    G[, k] <- (gpd_nll_obs(pp, y) - gpd_nll_obs(pm, y)) / (2 * h[k])
  }
  Gy <- rowsum(G, group = grp)                 # yearly score sums
  Gy <- sweep(Gy, 2, colMeans(Gy))             # centre (total score is ~ 0)
  V  <- crossprod(Gy)
  Hi <- tryCatch(solve(fit$hessian), error = function(e) NULL)
  if (is.null(Hi)) return(c(sigma = NA_real_, xi = NA_real_))
  S  <- Hi %*% V %*% Hi
  se <- sqrt(pmax(diag(S), 0))
  c(sigma = fit$sigma * se[1], xi = se[2])     # delta method for sigma
}

## Ferro-Segers (2003) intervals estimator of the extremal index.
## times: integer day indices of the exceedances, in increasing order.
extremal_index_fs <- function(times) {
  n <- length(times)
  if (n < 3) return(NA_real_)
  D <- diff(times)
  th <- if (max(D) <= 2) {
    2 * sum(D)^2 / ((n - 1) * sum(D^2))
  } else {
    2 * sum(D - 1)^2 / ((n - 1) * sum((D - 1) * (D - 2)))
  }
  min(1, th)
}

## POT return level with extremal-index correction (Lee & Lee 2020, eq. (3),
## written through the exceedance rate): rate = exceedances per year, and the
## N-year level solves  rate * theta * P(excess > x - u) = 1/N.
pot_return_level <- function(N, u, sigma, xi, rate, theta = 1) {
  m <- N * rate * theta
  if (abs(xi) < 1e-8) return(u + sigma * log(m))
  u + sigma / xi * (m^xi - 1)
}

## Runs declustering: exceedances separated by more than r days start a new
## cluster; the peak of each cluster is kept.
decluster_runs <- function(x, day_index, u, r) {
  exc <- which(!is.na(x) & x > u)
  if (length(exc) == 0)
    return(list(peaks = numeric(0), times = integer(0), n_clusters = 0L))
  t_exc <- day_index[exc]
  cl_id <- cumsum(c(TRUE, diff(t_exc) > r))
  xs    <- x[exc]
  peaks <- as.numeric(tapply(xs, cl_id, max))
  list(peaks = peaks, n_clusters = max(cl_id))
}

## Stationary marginal analysis for one variable: all daily exceedances kept,
## clustering handled through the extremal index (the mode-1 route).
marginal_stationary <- function(x, day_index, wy, tau, years_span, N_ret) {
  ok  <- !is.na(x)
  u   <- as.numeric(quantile(x[ok], probs = tau))
  exc <- ok & x > u
  y   <- x[exc] - u
  fit <- fit_gpd(y)
  se_sw <- smith_sandwich_gpd(fit, y, grp = wy[exc])
  th    <- extremal_index_fs(day_index[exc])
  rate  <- sum(exc) / years_span
  rl <- sapply(N_ret, pot_return_level, u = u, sigma = fit$sigma, xi = fit$xi,
               rate = rate, theta = th)
  names(rl) <- paste0("RL", N_ret)
  list(u = u, tau = tau, n_exc = sum(exc), rate = rate, theta = th,
       sigma = fit$sigma, xi = fit$xi,
       se_naive = fit$se_naive, se_smith = se_sw, fit = fit, rl = rl)
}

## Declustered variant (mode-2 robustness check): GP fitted to cluster-peak
## excesses, return level through the cluster rate, no theta correction.
marginal_declustered <- function(x, day_index, u, r, years_span, N_ret) {
  dc  <- decluster_runs(x, day_index, u, r)
  y   <- dc$peaks - u
  fit <- fit_gpd(y)
  rate_cl <- dc$n_clusters / years_span
  rl <- sapply(N_ret, pot_return_level, u = u, sigma = fit$sigma, xi = fit$xi,
               rate = rate_cl, theta = 1)
  names(rl) <- paste0("RL", N_ret)
  list(r = r, n_clusters = dc$n_clusters, rate = rate_cl,
       sigma = fit$sigma, xi = fit$xi, rl = rl, fit = fit)
}

## ============================================== S9a: non-stationary threshold

## Quantile-regression threshold u(t) = u0 + beta * tdec, with tdec in decades
## since record start -- the linear special case (Lee & Lee 2020) of the
## covariate-dependent threshold of Jonathan et al. (2014), Section 3.2.
## Uses quantreg::rq when available; otherwise a direct pinball-loss fit
## (point estimate only, no standard error).
fit_ns_threshold <- function(x, tdec, tau, do_se = TRUE, R_boot = 50) {
  ok <- !is.na(x)
  if (requireNamespace("quantreg", quietly = TRUE)) {
    xv <- x[ok]; td <- tdec[ok]
    fq <- quantreg::rq(xv ~ td, tau = tau)
    co <- as.numeric(coef(fq))
    seb <- NA_real_
    if (do_se)
      seb <- tryCatch(summary(fq, se = "boot", R = R_boot)$coefficients[2, 2],
                      error = function(e) NA_real_)
  } else {
    pin <- function(p) { r <- x[ok] - p[1] - p[2] * tdec[ok]
                         sum(r * (tau - (r < 0))) }
    o   <- optim(c(as.numeric(quantile(x[ok], tau)), 0), pin,
                 method = "Nelder-Mead", control = list(maxit = 5000))
    co  <- o$par; seb <- NA_real_
  }
  list(u0 = co[1], beta = co[2], se_beta = seb, u_t = co[1] + co[2] * tdec)
}

## Non-stationary marginal analysis: moving threshold u(t), constant GP scale
## and shape above it (the Lee & Lee configuration); theta and the exceedance
## rate computed from the exceedances of the moving threshold; return levels
## evaluated at reference times eval_tdec.
marginal_ns <- function(x, day_index, wy, tdec, tau, years_span, N_ret,
                        eval_tdec, R_boot = 50) {
  nst <- fit_ns_threshold(x, tdec, tau, do_se = TRUE, R_boot = R_boot)
  exc <- !is.na(x) & x > nst$u_t
  y   <- x[exc] - nst$u_t[exc]
  fit <- fit_gpd(y)
  se_sw <- smith_sandwich_gpd(fit, y, grp = wy[exc])
  th    <- extremal_index_fs(day_index[exc])
  rate  <- sum(exc) / years_span
  rl <- sapply(eval_tdec, function(td)
    sapply(N_ret, pot_return_level,
           u = nst$u0 + nst$beta * td, sigma = fit$sigma, xi = fit$xi,
           rate = rate, theta = th))
  rownames(rl) <- paste0("RL", N_ret)
  list(u0 = nst$u0, beta = nst$beta, se_beta = nst$se_beta,
       n_exc = sum(exc), rate = rate, theta = th,
       sigma = fit$sigma, xi = fit$xi,
       se_naive = fit$se_naive, se_smith = se_sw, rl = rl, fit = fit)
}

## ============================================================= S4: transform

## Semiparametric marginal df (HT04 eq. (1.3)): empirical below u with the
## n/(n+1) convention, GP tail above u. Returns vectorized F and Q; the pair
## is used for the Gumbel transform and its inverse (HT04 eq. (1.4)).
make_transform <- function(x, u, sigma, xi) {
  x  <- x[!is.na(x)]
  n  <- length(x)
  sx <- sort(x)
  pp <- (1:n) / (n + 1)
  pu <- mean(x > u)
  Fe <- approxfun(sx, pp, rule = 2, ties = mean)
  Qe <- approxfun(pp, sx, rule = 2, ties = mean)
  Ffun <- function(q) {
    out <- Fe(q)
    hi  <- !is.na(q) & q > u
    if (any(hi)) {
      if (abs(xi) < 1e-8) {
        out[hi] <- 1 - pu * exp(-(q[hi] - u) / sigma)
      } else {
        z <- 1 + xi * (q[hi] - u) / sigma
        out[hi] <- ifelse(z > 0, 1 - pu * z^(-1 / xi), 1)
      }
    }
    pmin(pmax(out, 1e-12), 1 - 1e-12)
  }
  Qfun <- function(p) {
    out <- Qe(p)
    hi  <- !is.na(p) & p > 1 - pu
    if (any(hi)) {
      if (abs(xi) < 1e-8) {
        out[hi] <- u + sigma * log(pu / (1 - p[hi]))
      } else {
        out[hi] <- u + sigma / xi * (((1 - p[hi]) / pu)^(-xi) - 1)
      }
    }
    out
  }
  list(F = Ffun, Q = Qfun, u = u, sigma = sigma, xi = xi, pu = pu)
}

to_gumbel   <- function(p) -log(-log(p))
from_gumbel <- function(y) exp(-exp(-y))

## ==================================================== S5: HT dependence model

## Gaussian-estimation objective (HT04 eq. (5.2)) for one conditioned
## component:  Y_j = a y + y^b (mu + s Z), working model Z ~ N(0,1).
## par = c(eta_a, eta_b, mu, log s) with a = plogis(eta_a) in (0,1) and
## b = 1 - exp(-eta_b) in (-Inf, 1)  (positive-dependence branch of HT04).
ht_nll <- function(par, y, yj) {
  a  <- plogis(par[1])
  b  <- 1 - exp(-par[2])
  mu <- par[3]
  s  <- exp(par[4])
  m  <- a * y + mu * y^b
  sd <- s * y^b
  sum(log(sd) + 0.5 * ((yj - m) / sd)^2)
}

ht_fit_pair <- function(y, yj) {
  best <- NULL
  for (a0 in c(0.15, 0.5, 0.85)) for (b0 in c(-0.3, 0.1, 0.45)) {
    r0 <- (yj - a0 * y) / y^b0
    p0 <- c(qlogis(a0), -log(1 - b0), mean(r0), log(max(sd(r0), 1e-3)))
    o  <- tryCatch(optim(p0, ht_nll, y = y, yj = yj, method = "Nelder-Mead",
                         control = list(maxit = 2000)),
                   error = function(e) NULL)
    if (!is.null(o) && (is.null(best) || o$value < best$value)) best <- o
  }
  o2 <- tryCatch(optim(best$par, ht_nll, y = y, yj = yj, method = "BFGS",
                       control = list(maxit = 500)),
                 error = function(e) NULL)
  if (!is.null(o2) && o2$value <= best$value) best <- o2
  a  <- plogis(best$par[1]); b <- 1 - exp(-best$par[2])
  mu <- best$par[3];         s <- exp(best$par[4])
  z  <- (yj - a * y) / y^b       # HT04 residuals; their empirical law is G
  if (a < 0.02 && b < 0)
    warning("a ~ 0 with b < 0: negative-dependence (c,d) branch of HT04 not implemented; not expected for flow data")
  list(a = a, b = b, mu = mu, s = s, nll = best$value,
       conv = best$convergence, z = z)
}

## Fit the conditional model for one conditioning variable. Ygum: matrix on
## the Gumbel scale (NA where the data were NA); only complete rows enter, so
## the residual rows stay aligned across conditioned components and can be
## resampled jointly (preserving the dependence of Z that the working
## independence assumption ignores during point estimation).
ht_fit <- function(Ygum, cond, kappa = 0.95) {
  v   <- to_gumbel(kappa)
  ok  <- stats::complete.cases(Ygum)
  sel <- ok & Ygum[, cond] > v
  sel[is.na(sel)] <- FALSE
  Yc  <- Ygum[sel, , drop = FALSE]
  y   <- Yc[, cond]
  others <- setdiff(colnames(Ygum), cond)
  fits <- list()
  Z <- matrix(NA_real_, nrow(Yc), length(others),
              dimnames = list(NULL, others))
  for (j in others) {
    f <- ht_fit_pair(y, Yc[, j])
    fits[[j]] <- f
    Z[, j] <- f$z
  }
  list(v = v, kappa = kappa, cond = cond, n = nrow(Yc), fits = fits, Z = Z)
}

## ================================================= S7: conditional simulation

## HT04 Section 4.3 sampling algorithm, conditioning on X_cond exceeding
## cond_level on the original scale:
##   1. draw Y_cond from the Gumbel distribution above the transformed level
##   2. draw a residual ROW from the stored set, independently of Y_cond
##   3. Y_j = a_j Y + Y^{b_j} Z_j componentwise
##   4. back-transform each component with its own inverse semiparametric df
ht_cond_sim <- function(hf, transforms, cond_level, n_sim = 5e4) {
  Fc <- transforms[[hf$cond]]$F
  yc <- to_gumbel(Fc(cond_level))
  if (yc < hf$v)
    warning("conditioning level lies below the dependence threshold; the HT model was not fitted there")
  Fv  <- from_gumbel(yc)
  p   <- Fv + runif(n_sim) * (1 - Fv)
  yi  <- to_gumbel(p)
  idx <- sample(nrow(hf$Z), n_sim, replace = TRUE)
  sim <- sapply(names(hf$fits), function(j) {
    f  <- hf$fits[[j]]
    yj <- f$a * yi + yi^f$b * hf$Z[idx, j]
    transforms[[j]]$Q(from_gumbel(yj))
  })
  colnames(sim) <- names(hf$fits)
  sim
}

## ======================================== S8: water-year bootstrap replicate

## One block-bootstrap replicate: resample water years with replacement and
## concatenate. day_index is re-sequenced (interexceedance times for theta);
## tdec keeps each day's ORIGINAL value so the trend regression pairs (x, t)
## survive the resampling. Thresholds are re-estimated as quantiles inside
## each replicate; note that threshold-CHOICE uncertainty (the level tau) is
## still outside the bands, as in HT04 Section 5.4.
boot_one <- function(dat, vars, cond_var, tau, kappa, years_span,
                     N_track = 50) {
  wys  <- unique(dat$wy)
  pick <- sample(wys, length(wys), replace = TRUE)
  idx  <- unlist(lapply(pick, function(w) which(dat$wy == w)),
                 use.names = FALSE)
  db   <- dat[idx, , drop = FALSE]
  db$day_index <- seq_len(nrow(db))
  out <- c()
  Yg  <- matrix(NA_real_, nrow(db), length(vars),
                dimnames = list(NULL, vars))
  for (v in vars) {
    x  <- db[[v]]
    ms <- marginal_stationary(x, db$day_index, db$wy, tau, years_span,
                              N_ret = N_track)
    out[paste0(v, "_RL", N_track)] <- ms$rl[1]
    out[paste0(v, "_sigma")] <- ms$sigma
    out[paste0(v, "_xi")]    <- ms$xi
    out[paste0(v, "_theta")] <- ms$theta
    nb <- fit_ns_threshold(x, db$tdec, tau, do_se = FALSE)
    out[paste0(v, "_beta_u")] <- nb$beta
    tr <- make_transform(x, ms$u, ms$sigma, ms$xi)
    Yg[, v] <- to_gumbel(tr$F(x))
  }
  hf <- suppressWarnings(ht_fit(Yg, cond = cond_var, kappa = kappa))
  for (j in names(hf$fits)) {
    out[paste0("a_", j)] <- hf$fits[[j]]$a
    out[paste0("b_", j)] <- hf$fits[[j]]$b
  }
  out
}
