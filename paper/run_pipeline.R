## run_pipeline.R --------------------------------------------------------------
## End-to-end run: read daily data -> prepare -> S3 stationary marginal POT
## (all exceedances + extremal index; declustered robustness variant) ->
## S9a non-stationary threshold marginal (linear quantile regression) ->
## S4 Gumbel transform -> S5 HT conditional dependence model ->
## S7 conditional simulation -> S8 optional water-year block bootstrap.
## Prints model parameters and return levels only; no plots, no analysis.
##
## Easiest way to run everything: use RUN_ALL.R (one click / one command).
## Manual alternative:  Rscript make_fake_data.R   then   Rscript run_pipeline.R

source("extremes_functions.R")
options(width = 200)   # keep printed tables on one line

## ------------------------------------------------------------------ config ---
DATA_CSV <- "fake_daily_flows.csv"   # <- point at the real export later
                                     #    required columns: date, station, sf, bf
## modelled vector (Option A default): sf at four stations + bf at station 1;
## edit the mapping when the real station IDs are known
VAR_DEF <- list(
  X1 = c(station = "S1", field = "sf"),
  X2 = c(station = "S2", field = "sf"),
  X3 = c(station = "S3", field = "sf"),
  X4 = c(station = "S4", field = "sf"),
  B1 = c(station = "S1", field = "bf")
)
COND_VAR   <- "X1"      # conditioning variable of the HT model
TAU        <- 0.95      # marginal threshold quantile
KAPPA      <- 0.95      # dependence threshold quantile (Gumbel scale)
N_RET      <- c(10, 25, 50, 100)   # return periods, years
EVAL_YEARS <- c(1970, 1990, 2012)  # years at which NS return levels are shown
COND_N     <- 10        # conditioning event: COND_VAR above its COND_N-yr level
N_SIM      <- 5e4       # Monte Carlo size for conditional functionals
RUN_BOOTSTRAP <- FALSE  # TRUE -> water-year block bootstrap (slower)
B_BOOT     <- 200       # bootstrap replicates (raise to 1000+ for final runs)
SEED       <- 1
## -----------------------------------------------------------------------------

set.seed(SEED)

## S1 -- import -----------------------------------------------------------------
raw <- read.csv(DATA_CSV, stringsAsFactors = FALSE)
stopifnot(all(c("date", "station", "sf", "bf") %in% names(raw)))
raw$date <- as.Date(raw$date)

dates <- sort(unique(raw$date))
dat   <- data.frame(date = dates)
for (v in names(VAR_DEF)) {
  st  <- unname(VAR_DEF[[v]]["station"])
  fld <- unname(VAR_DEF[[v]]["field"])
  sub <- raw[raw$station == st, ]
  dat[[v]] <- sub[[fld]][match(dates, sub$date)]
}
vars <- names(VAR_DEF)

## S1 -- prepare ----------------------------------------------------------------
dat$year  <- as.integer(format(dat$date, "%Y"))
dat$month <- as.integer(format(dat$date, "%m"))
dat$wy    <- dat$year + (dat$month >= 10)  # water year Oct-Sep, end-year label
dat$day_index <- as.integer(dat$date - min(dat$date)) + 1L
dat$tdec  <- (dat$day_index - 1) / 365.25 / 10   # decades since record start
years_span <- as.numeric(diff(range(dat$date)) + 1) / 365.25
eval_tdec  <- as.numeric(as.Date(paste0(EVAL_YEARS, "-07-01")) -
                         min(dat$date)) / 365.25 / 10

cat("record:", format(min(dat$date)), "to", format(max(dat$date)),
    sprintf("(%.1f years) | variables: %s | missing rate: %s\n",
            years_span, paste(vars, collapse = " "),
            paste(sprintf("%s %.1f%%", vars,
                          100 * sapply(vars, function(v) mean(is.na(dat[[v]])))),
                  collapse = ", ")))

## S3 -- stationary marginal POT (all exceedances + extremal index) -------------
marg <- lapply(vars, function(v)
  marginal_stationary(dat[[v]], dat$day_index, dat$wy, TAU, years_span, N_RET))
names(marg) <- vars

tab_marg <- do.call(rbind, lapply(vars, function(v) {
  m <- marg[[v]]
  data.frame(var = v, u = m$u, n_exc = m$n_exc,
             sigma = m$sigma, se_sig_naive = m$se_naive["sigma"],
             se_sig_smith = m$se_smith["sigma"],
             xi = m$xi, se_xi_naive = m$se_naive["xi"],
             se_xi_smith = m$se_smith["xi"],
             theta = m$theta, row.names = NULL)
}))
tab_rl <- do.call(rbind, lapply(vars, function(v)
  data.frame(var = v, t(marg[[v]]$rl), row.names = NULL)))

cat("\n== S3 stationary marginal GP fits (threshold = ", TAU,
    " quantile; theta = Ferro-Segers extremal index) ==\n", sep = "")
print(format(tab_marg, digits = 3), row.names = FALSE)
cat("\n== S3 return levels, all-exceedances fit with extremal-index correction ==\n")
print(format(tab_rl, digits = 4), row.names = FALSE)

## S3 -- declustered robustness run (r anchored at 1/theta) ---------------------
decl <- lapply(vars, function(v) {
  m <- marg[[v]]
  r <- max(2, ceiling(1 / m$theta))
  marginal_declustered(dat[[v]], dat$day_index, m$u, r, years_span, N_RET)
})
names(decl) <- vars
tab_decl <- do.call(rbind, lapply(vars, function(v) {
  d <- decl[[v]]
  data.frame(var = v, r = d$r, n_clusters = d$n_clusters,
             sigma = d$sigma, xi = d$xi, t(d$rl), row.names = NULL)
}))
cat("\n== S3 robustness: runs-declustered fits (cluster peaks only) ==\n")
print(format(tab_decl, digits = 4), row.names = FALSE)

## S9a -- non-stationary threshold marginal (linear quantile regression) --------
ns <- lapply(vars, function(v)
  marginal_ns(dat[[v]], dat$day_index, dat$wy, dat$tdec, TAU, years_span,
              N_RET, eval_tdec))
names(ns) <- vars
tab_ns <- do.call(rbind, lapply(vars, function(v) {
  m <- ns[[v]]
  data.frame(var = v, u0 = m$u0, beta_per_decade = m$beta,
             se_beta = m$se_beta, sigma = m$sigma, xi = m$xi,
             theta = m$theta, n_exc = m$n_exc, row.names = NULL)
}))
cat("\n== S9a NS threshold u(t) = u0 + beta * (decades since start); constant GP above ==\n")
print(format(tab_ns, digits = 3), row.names = FALSE)

tab_ns_rl <- do.call(rbind, lapply(vars, function(v) {
  m <- ns[[v]]
  d <- as.data.frame(t(m$rl))
  d$year <- EVAL_YEARS; d$var <- v
  d[, c("var", "year", paste0("RL", N_RET))]
}))
cat("\n== S9a NS return levels evaluated at reference years ==\n")
print(format(tab_ns_rl, digits = 4), row.names = FALSE)

## S4 -- transform to Gumbel margins (stationary marginal model) ----------------
transforms <- lapply(vars, function(v)
  make_transform(dat[[v]], marg[[v]]$u, marg[[v]]$sigma, marg[[v]]$xi))
names(transforms) <- vars
Ygum <- sapply(vars, function(v) to_gumbel(transforms[[v]]$F(dat[[v]])))
for (v in vars) Ygum[is.na(dat[[v]]), v] <- NA

## S5 -- HT conditional dependence model ----------------------------------------
hf <- ht_fit(Ygum, cond = COND_VAR, kappa = KAPPA)
tab_ht <- do.call(rbind, lapply(names(hf$fits), function(j) {
  f <- hf$fits[[j]]
  data.frame(conditioned = j, a = f$a, b = f$b, mu = f$mu, s = f$s,
             conv = f$conv, row.names = NULL)
}))
cat("\n== S5 HT dependence model | conditioning: ", COND_VAR, " > Gumbel ",
    KAPPA, " quantile (v = ", round(hf$v, 3), "), n_dep = ", hf$n,
    " ==\n", sep = "")
print(format(tab_ht, digits = 3), row.names = FALSE)

## S7 -- conditional simulation --------------------------------------------------
cond_level <- as.numeric(marg[[COND_VAR]]$rl[paste0("RL", COND_N)])
sim <- ht_cond_sim(hf, transforms, cond_level, n_sim = N_SIM)
targets <- sapply(names(hf$fits), function(j)
  as.numeric(marg[[j]]$rl[paste0("RL", COND_N)]))
cond_prob <- colMeans(sweep(sim, 2, targets, ">"), na.rm = TRUE)
tab_cp <- data.frame(conditioned = names(hf$fits),
                     own_RL = as.numeric(targets),
                     P_exceed_given_cond = as.numeric(cond_prob))
cat("\n== S7 conditional functionals: P( X_j > its own ", COND_N,
    "-yr level | ", COND_VAR, " > its ", COND_N, "-yr level = ",
    round(cond_level, 2), " ) ==\n", sep = "")
print(format(tab_cp, digits = 3), row.names = FALSE)

## S8 -- optional water-year block bootstrap ------------------------------------
boot_ci <- NULL
if (RUN_BOOTSTRAP) {
  cat("\nrunning water-year block bootstrap, B =", B_BOOT, "...\n")
  bs <- t(replicate(B_BOOT,
           boot_one(dat, vars, COND_VAR, TAU, KAPPA, years_span,
                    N_track = 50)))
  boot_ci <- t(apply(bs, 2, quantile, probs = c(0.025, 0.5, 0.975),
                     na.rm = TRUE))
  cat("\n== S8 bootstrap percentile intervals (2.5%, 50%, 97.5%) ==\n")
  print(round(boot_ci, 3))
}

## export every printed table as a CSV file -------------------------------------
dir.create("results_tables", showWarnings = FALSE)
write.csv(tab_marg,  "results_tables/S3_marginal_fits.csv",          row.names = FALSE)
write.csv(tab_rl,    "results_tables/S3_return_levels.csv",          row.names = FALSE)
write.csv(tab_decl,  "results_tables/S3_declustered_robustness.csv", row.names = FALSE)
write.csv(tab_ns,    "results_tables/S9a_ns_threshold_fits.csv",     row.names = FALSE)
write.csv(tab_ns_rl, "results_tables/S9a_ns_return_levels.csv",      row.names = FALSE)
write.csv(tab_ht,    "results_tables/S5_HT_dependence.csv",          row.names = FALSE)
write.csv(tab_cp,    "results_tables/S7_conditional_probs.csv",      row.names = FALSE)
if (!is.null(boot_ci))
  write.csv(data.frame(quantity = rownames(boot_ci), boot_ci,
                       check.names = FALSE),
            "results_tables/S8_bootstrap_CIs.csv", row.names = FALSE)
cat("\nwritten: results_tables/*.csv\n")

## save everything for the later evaluation stage -------------------------------
results <- list(config = list(tau = TAU, kappa = KAPPA, N_ret = N_RET,
                              cond_var = COND_VAR, cond_N = COND_N,
                              eval_years = EVAL_YEARS, seed = SEED),
                marginal_stationary = marg,
                marginal_declustered = decl,
                marginal_ns = ns,
                ht = hf,
                transforms = transforms,
                cond_sim_prob = tab_cp,
                boot_ci = boot_ci)
saveRDS(results, "pipeline_results.rds")
cat("\nsaved: pipeline_results.rds\n")
