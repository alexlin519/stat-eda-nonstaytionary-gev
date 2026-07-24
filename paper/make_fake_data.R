## make_fake_data.R -----------------------------------------------------------
## Synthetic daily streamflow (sf) and baseflow (bf), 4 stations, 1968-2012.
## Output: fake_daily_flows.csv with columns  date, station, sf, bf
##
## When the real data arrive: export them to a CSV with exactly these four
## columns (one row per station-day) and point run_pipeline.R at that file.
## Nothing else in the pipeline changes.
##
## Planted structure (so the pipeline has known things to recover):
##   - seasonal freshet bump per station (Gaussian in day-of-year)
##   - freshet amplitude grows 25% over the record  -> upward trend in the
##     high quantiles, recoverable by the quantile-regression threshold beta
##   - shared regional AR(1) factor -> cross-station dependence (HT a > 0)
##   - station AR(1) noise with t(6) innovations -> clustered exceedances
##     (extremal index < 1) and a mildly heavy tail (GP xi > 0)
##   - baseflow from a one-parameter recursive filter -> bf <= sf, strongly
##     dependent on sf at the same station
##   - injected missing stretches per station

set.seed(42)

dates <- seq(as.Date("1968-01-01"), as.Date("2012-12-31"), by = "day")
nd    <- length(dates)
doy   <- as.integer(format(dates, "%j"))
year  <- as.integer(format(dates, "%Y"))
yfrac <- (year - 1968) / (2012 - 1968)            # 0 at start, 1 at end

n_st  <- 4
base  <- log(c(60, 40, 90, 25))                    # station scale, log m3/s
A0    <- c(1.6, 1.4, 1.8, 1.5)                     # freshet amplitude (log units)
peakd <- c(150, 160, 145, 155)                     # freshet peak day-of-year
wd    <- 35                                        # freshet width (days)
trend_amp <- 0.25                                  # planted amplitude growth
lam   <- 0.8                                       # loading on shared factor

## AR(1) with unit-variance t(6) innovations scaled to sd_innov
ar1 <- function(n, phi, sd_innov, df = 6) {
  x <- numeric(n)
  innov <- sd_innov * rt(n, df = df) / sqrt(df / (df - 2))
  for (t in 2:n) x[t] <- phi * x[t - 1] + innov[t]
  x
}

f <- ar1(nd, phi = 0.70, sd_innov = 0.20)          # shared regional driver

sf <- matrix(NA_real_, nd, n_st)
for (s in 1:n_st) {
  e    <- ar1(nd, phi = 0.75, sd_innov = 0.25)
  seas <- A0[s] * (1 + trend_amp * yfrac) * exp(-((doy - peakd[s])^2) / (2 * wd^2))
  sf[, s] <- exp(base[s] + 0.05 * yfrac + seas + lam * f + e)
}

## baseflow: one-parameter recursive filter (Lyne-Hollick style); bf <= sf
lh_baseflow <- function(q, a = 0.925) {
  n  <- length(q)
  bf <- rep(NA_real_, n)
  bf[1] <- q[1]
  for (t in 2:n) {
    if (is.na(q[t]))                        { bf[t] <- NA_real_; next }
    if (is.na(bf[t - 1]) || is.na(q[t - 1])) { bf[t] <- q[t];    next }
    bf[t] <- min(q[t], a * bf[t - 1] + ((1 - a) / 2) * (q[t] + q[t - 1]))
  }
  bf
}

## inject missing stretches: a few short gaps plus one long gap per station
poke_na <- function(x, k_gaps = 3, len_range = c(5, 30), long_gap = 60) {
  n <- length(x)
  for (g in seq_len(k_gaps)) {
    st <- sample(1:(n - 40), 1)
    x[st:(st + sample(len_range[1]:len_range[2], 1))] <- NA
  }
  st <- sample(1:(n - long_gap - 1), 1)
  x[st:(st + long_gap)] <- NA
  x
}
for (s in 1:n_st) sf[, s] <- poke_na(sf[, s])

bf <- sapply(1:n_st, function(s) lh_baseflow(sf[, s]))

out <- data.frame(
  date    = rep(dates, n_st),
  station = rep(sprintf("S%d", 1:n_st), each = nd),
  sf      = as.vector(sf),
  bf      = as.vector(bf)
)
write.csv(out, "fake_daily_flows.csv", row.names = FALSE)
cat("written fake_daily_flows.csv:", nrow(out), "rows,",
    sum(is.na(out$sf)), "missing sf values\n")
