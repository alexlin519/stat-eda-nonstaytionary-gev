library(extRemes)
#library(ismev)
library(dplyr)
library(ggplot2)
library(gridExtra)

# 1. DATA PREPARATION
sf_hist <- read.csv("sf_yearly_max_hist.csv")
target_stations <- unique(sf_hist$Station)[1:10]
sf_subset <- sf_hist %>% filter(Station %in% target_stations)

gev_results <- list()
summary_list <- list()
ts_plot_data <- data.frame()
help(return.level)
# 2. MAIN FITTING LOOP
for (stn in target_stations) {
  
  stn_data <- sf_subset %>% filter(Station == stn)
  stn_data <- na.omit(stn_data)
  stn_data$Year_centered <- stn_data$Year - mean(stn_data$Year)
  
  # Stationary Model
  fit_stat <- fevd(x = MaxFlow, data = stn_data, type = "GEV")
  st_est <- summary(fit_stat, silent = TRUE)$par
  st_se  <- summary(fit_stat, silent = TRUE)$se
  st_rl  <- return.level(fit_stat, return.period = c(50, 100), do.ci = FALSE)
  
  # Non-Stationary Model
  fit_nonstat <- fevd(x = MaxFlow, data = stn_data, type = "GEV",
                      location.fun = ~Year_centered,
                      scale.fun = ~Year_centered)
  ns_est <- summary(fit_nonstat, silent = TRUE)$par
  ns_rl_matrix <- return.level(fit_nonstat, return.period = c(50, 100), do.ci = FALSE)
  ns_rl_final  <- ns_rl_matrix[nrow(ns_rl_matrix), ]
  
  # Store Time Series Data
  stn_data$Mu_Trend <- findpars(fit_nonstat)$location
  ts_plot_data <- bind_rows(ts_plot_data, stn_data)
  
  # Build Summary Row
  row_df <- data.frame(
    Station = stn,
    Stat_Mu_Est    = round(st_est["location"], 2),
    Stat_Mu_SE     = round(st_se["location"], 2),
    Stat_Sigma_Est = round(st_est["scale"], 2),
    Stat_Sigma_SE  = round(st_se["scale"], 2),
    Stat_Xi_Est    = round(st_est["shape"], 3),
    Stat_Xi_SE     = round(st_se["shape"], 3),
    Stat_RL_50     = round(as.numeric(st_rl[1]), 2),
    Stat_RL_100    = round(as.numeric(st_rl[2]), 2),
    NS_Mu0_Est     = round(ns_est["mu0"], 2),
    NS_Mu1_Trend   = round(ns_est["mu1"], 2),
    NS_Sig0_Est    = round(ns_est["scale0"], 2),
    NS_Sig1_Trend  = round(ns_est["scale1"], 2),
    NS_Xi_Est      = round(ns_est["shape"], 3),
    NS_RL_50_Final = round(as.numeric(ns_rl_final[1]), 2),
    NS_RL_100_Final= round(as.numeric(ns_rl_final[2]), 2)
  )
  
  summary_list[[stn]] <- row_df
  
  # Scrub the call parameter to remove the fevd text from the final plots
  fit_stat$call <- NULL
  fit_nonstat$call <- NULL
  
  gev_results[[stn]] <- list(Stat = fit_stat, NonStat = fit_nonstat)
}

# 3. COMPILE CI TABLE
master_summary <- bind_rows(summary_list) %>%
  mutate(
    Stat_Mu_Lower = round(Stat_Mu_Est - 1.96 * Stat_Mu_SE, 2),
    Stat_Mu_Upper = round(Stat_Mu_Est + 1.96 * Stat_Mu_SE, 2),
    Stat_Sig_Lower = round(Stat_Sigma_Est - 1.96 * Stat_Sigma_SE, 2),
    Stat_Sig_Upper = round(Stat_Sigma_Est + 1.96 * Stat_Sigma_SE, 2),
    Stat_Xi_Lower = round(Stat_Xi_Est - 1.96 * Stat_Xi_SE, 3),
    Stat_Xi_Upper = round(Stat_Xi_Est + 1.96 * Stat_Xi_SE, 3)
  )
rownames(master_summary) <- NULL


# 4. INITIALIZE PDF EXPORT
pdf("Complete_GEV_Analysis.pdf", width = 11, height = 8.5)

# ---------------------------------------------------------
# PAGE 1: EQUATION COVER PAGE
# ---------------------------------------------------------
par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), axes = FALSE, xlab = "", ylab = "")

text(5, 9, "Generalized Extreme Value (GEV) Analysis Equations", cex = 2)

text(5, 7.5, "Stationary Model GEV Distribution Function:", cex = 1.2)
text(5, 6.5, expression(G(z) == exp(- (1 + xi * (frac(z - mu, sigma)))^{-1/xi})), cex = 1.5)

text(5, 5.0, "Stationary Return Level (p = 1/T):", cex = 1.2)
text(5, 4.0, expression(z[p] == mu - frac(sigma, xi) * (1 - (-log(1-p))^{-xi})), cex = 1.5)

text(5, 2.5, "Non-Stationary Linear Parameters:", cex = 1.2)
text(5, 1.5, expression(paste(mu(t) == mu[0] + mu[1] * (t - bar(t)), "    ", sigma(t) == sigma[0] + sigma[1] * (t - bar(t)))), cex = 1.5)

# ---------------------------------------------------------
# PAGE 2 & 3: SUMMARY TABLES
# ---------------------------------------------------------
table_theme <- ttheme_default(base_size = 8)

stat_cols <- c("Station", "Stat_Mu_Est", "Stat_Mu_SE", "Stat_Mu_Lower", "Stat_Mu_Upper", 
               "Stat_Sigma_Est", "Stat_Sigma_SE", "Stat_Sig_Lower", "Stat_Sig_Upper", 
               "Stat_Xi_Est", "Stat_Xi_SE", "Stat_Xi_Lower", "Stat_Xi_Upper")
grid.arrange(top = "Stationary GEV Parameters and 95% CI", 
             tableGrob(master_summary[, stat_cols], theme = table_theme, rows = NULL))

ns_rl_cols <- c("Station", "NS_Mu0_Est", "NS_Mu1_Trend", "NS_Sig0_Est", "NS_Sig1_Trend", 
                "NS_Xi_Est", "Stat_RL_50", "Stat_RL_100", "NS_RL_50_Final", "NS_RL_100_Final")
grid.arrange(top = "Non-Stationary Parameters and Return Levels", 
             tableGrob(master_summary[, ns_rl_cols], theme = table_theme, rows = NULL))

# ---------------------------------------------------------
# PAGES 4-6: TIME SERIES PLOTS
# ---------------------------------------------------------
p_all_10 <- ggplot(ts_plot_data, aes(x = Year)) +
  geom_line(aes(y = MaxFlow), color = "steelblue", alpha = 0.7) +
  geom_line(aes(y = Mu_Trend), color = "darkred", linetype = "dashed") +
  facet_wrap(~Station, scales = "free_y", ncol = 5) +
  labs(title = expression(paste("All 10 Stations: Annual Maxima vs Non-Stationary Trend (", mu, ")")),
       y = "Max Flow") +
  theme_minimal()
print(p_all_10)

stations_group1 <- target_stations[1:5]
stations_group2 <- target_stations[6:10]

p_first_5 <- ggplot(ts_plot_data %>% filter(Station %in% stations_group1), aes(x = Year)) +
  geom_line(aes(y = MaxFlow), color = "steelblue") +
  geom_line(aes(y = Mu_Trend), color = "darkred", linetype = "dashed") +
  facet_wrap(~Station, scales = "free_y", ncol = 1) +
  labs(title = expression(paste("Stations 1-5: Annual Maxima vs Non-Stationary Trend (", mu, ")")),
       y = "Max Flow") +
  theme_minimal()
print(p_first_5)

p_last_5 <- ggplot(ts_plot_data %>% filter(Station %in% stations_group2), aes(x = Year)) +
  geom_line(aes(y = MaxFlow), color = "steelblue") +
  geom_line(aes(y = Mu_Trend), color = "darkred", linetype = "dashed") +
  facet_wrap(~Station, scales = "free_y", ncol = 1) +
  labs(title = expression(paste("Stations 6-10: Annual Maxima vs Non-Stationary Trend (", mu, ")")),
       y = "Max Flow") +
  theme_minimal()
print(p_last_5)

# ---------------------------------------------------------
# PAGE 7: PARAMETER CONFIDENCE INTERVAL PLOTS
# ---------------------------------------------------------
p_mu <- ggplot(master_summary, aes(x = reorder(Station, Stat_Mu_Est), y = Stat_Mu_Est)) +
  geom_point(color = "darkblue") +
  geom_errorbar(aes(ymin = Stat_Mu_Lower, ymax = Stat_Mu_Upper), width = 0.2) +
  coord_flip() + 
  labs(title = expression(paste("Location Parameter (", mu, ") with 95% CI")),
       x = "Station", y = "Estimate") +
  theme_minimal()

p_sig <- ggplot(master_summary, aes(x = reorder(Station, Stat_Sigma_Est), y = Stat_Sigma_Est)) +
  geom_point(color = "darkgreen") +
  geom_errorbar(aes(ymin = Stat_Sig_Lower, ymax = Stat_Sig_Upper), width = 0.2) +
  coord_flip() + 
  labs(title = expression(paste("Scale Parameter (", sigma, ") with 95% CI")),
       x = "Station", y = "Estimate") +
  theme_minimal()

p_xi <- ggplot(master_summary, aes(x = reorder(Station, Stat_Xi_Est), y = Stat_Xi_Est)) +
  geom_point(color = "darkred") +
  geom_errorbar(aes(ymin = Stat_Xi_Lower, ymax = Stat_Xi_Upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() + 
  labs(title = expression(paste("Shape Parameter (", xi, ") with 95% CI")),
       x = "Station", y = "Estimate") +
  theme_minimal()

grid.arrange(p_mu, p_sig, p_xi, ncol = 1)

# ---------------------------------------------------------
# PAGES 8+: DIAGNOSTICS & RETURN LEVEL CURVES PER STATION
# ---------------------------------------------------------
# ---------------------------------------------------------
# PAGES 8+: DIAGNOSTICS & RETURN LEVEL CURVES PER STATION
# ---------------------------------------------------------
for (stn in target_stations) {
  
  # Page A: Exactly 2 plots on one page (Stationary vs Non-Stationary Diagnostics)
  # Using QQ plots as the standard single-panel diagnostic to avoid the 8-panel clutter
  par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(5, 4, 4, 2))
  
  plot(gev_results[[stn]]$Stat, type = "qq", 
       main = "Stationary Diagnostics")
  
  plot(gev_results[[stn]]$NonStat, type = "qq", 
       main = "Non-Stationary Diagnostics")
  
  mtext(paste("Diagnostic Fit (QQ) for Station:", stn), outer = TRUE, cex = 1.5)
  
  # Page B: Exactly 1 plot on the next page (Stationary Return Level)
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5, 5, 4, 2))
  
  plot(gev_results[[stn]]$Stat, type = "rl", 
       main = paste("Stationary Return Level Curve:", stn), cex.main = 1.5)
}


# give me BAEZA ARNT7 BCHAL return plot side be side in one plot
stations_to_plot <- c("BAEZA", "ARNT7", "BCHAL")
par(mfrow = c(1, 3), oma = c(0, 0, 3, 0), mar = c(5, 5, 4, 2))
for (stn in stations_to_plot) {
  plot(gev_results[[stn]]$Stat, type = "rl",
       main = paste("Stationary Return Level Curve:", stn), cex.main = 1.5)
}
dev.off()

install.packages("ismev")
library(ismev)
stn_data <- sf_subset %>% filter(Station == "BAEZA")
#ger rid of first year because it is incomplete
stn_data <- stn_data %>% filter(Year > 1945)
stn_data <- na.omit(stn_data)
stn_data$Year_centered <- stn_data$Year - mean(stn_data$Year)
fit <- gev.fit(stn_data$MaxFlow)
summary(fit)
gev.diag(fit)



sf_subset <- sf_hist %>% filter(Station %in% target_stations)



stn_data2 <- sf_hist %>% filter(Station == "FRSPM")
#time serie plot
ggplot(stn_data2, aes(x = Year, y = MaxFlow)) +
  geom_line(color = "steelblue") +
  labs(title = "Station FRSPM: Annual Max Flow Time Series",
       x = "Year", y = "Max Flow") +
  theme_minimal()
#ger rid of first year because it is incomplete
stn_data2 <- stn_data2 %>% filter(Year > 1945)
stn_data2 <- na.omit(stn_data2)
stn_data2$Year_centered <- stn_data2$Year - mean(stn_data2$Year)
fit <- gev.fit(stn_data2$MaxFlow)
summary(fit)
gev.diag(fit)


stn_data3 <- sf_hist %>% filter(Station == "FRSMT")
ggplot(stn_data3, aes(x = Year, y = MaxFlow)) +
  geom_line(color = "steelblue") +
  labs(title = "Station FRSPM: Annual Max Flow Time Series",
       x = "Year", y = "Max Flow") +
  theme_minimal()
#ger rid of first year because it is incomplete
stn_data3 <- stn_data3 %>% filter(Year > 1945)
stn_data3 <- na.omit(stn_data3)
stn_data3$Year_centered <- stn_data3$Year - mean(stn_data3$Year)
fit <- gev.fit(stn_data3$MaxFlow)
summary(fit)
gev.diag(fit)



stn_data4 <- sf_hist %>% filter(Station == "FRSMS")
ggplot(stn_data4, aes(x = Year, y = MaxFlow)) +
  geom_line(color = "steelblue") +
  labs(title = "Station FRSMS: Annual Max Flow Time Series",
       x = "Year", y = "Max Flow") +
  theme_minimal()
#ger rid of first year because it is incomplete
stn_data4 <- stn_data4 %>% filter(Year > 1945)
stn_data4 <- na.omit(stn_data4)
stn_data4$Year_centered <- stn_data4$Year - mean(stn_data4$Year)
fit <- gev.fit(stn_data4$MaxFlow)
summary(fit)
gev.diag(fit)
#map of the staton 
# ts plot
# gev fit and value
# diag pllt 
#delta method of ci for return level 
# est error 
# corr 
# stionary and non stationary model       for nick 


#do all 10 stations in one loop
for (stn in target_stations) {
  stn_data <- sf_subset %>% filter(Station == stn)
  stn_data <- stn_data %>% filter(Year > 1945)
  stn_data <- na.omit(stn_data)
  stn_data$Year_centered <- stn_data$Year - mean(stn_data$Year)
  fit <- gev.fit(stn_data$MaxFlow)
  print(paste("Station:", stn))
  print(summary(fit))
  #get title of the station in the plot
  plot_title <- paste("GEV Diagnostics for Station:", stn)
  par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(5, 4, 4, 2))
  gev.diag(fit)
  #print the plot of gev.diag with the station name as the title
  mtext(plot_title, outer = TRUE, cex = 1.5)
}
# Stationary Model
fit_stat <- fevd(x = MaxFlow, data = stn_data, type = "GEV")
st_est <- summary(fit_stat, silent = TRUE)$par
st_se  <- summary(fit_stat, silent = TRUE)$se
st_rl  <- return.level(fit_stat, return.period = c(50, 100), do.ci = FALSE)
