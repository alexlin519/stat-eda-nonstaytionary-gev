# Load Required Libraries
library(dplyr)
library(ggplot2)
library(ismev)

# ==============================================================================
# CORE ANALYSIS FUNCTION
# ==============================================================================
run_gev_analysis <- function(station_name, data) {
  
  cat("\n======================================================\n")
  cat("ANALYSIS FOR STATION:", station_name, "\n")
  cat("======================================================\n")
  
  # ---------------------------------------------------------
  # PRE-PROCESSING
  # ---------------------------------------------------------
  # Filter for the specific station
  stn_data <- data %>% filter(Station == station_name)
  
  # Remove the year 1945 and keep data only up to 2012
  stn_data <- stn_data %>% filter(Year > 1945 & Year <= 2012)
  stn_data <- na.omit(stn_data)
  
  
  # ---------------------------------------------------------
  # ORDER 1: TIME SERIES PLOT
  # ---------------------------------------------------------
  ts_plot <- ggplot(stn_data, aes(x = Year, y = MaxFlow)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "darkblue", size = 2) +
    labs(title = paste("Station:", station_name, "- Annual Max Flow (1946-2012)"),
         x = "Year", y = "Max Flow") +
    theme_minimal()
  
  print(ts_plot)
  
  
  # ---------------------------------------------------------
  # ORDER 2: FIT GEV & DIAGNOSTIC PLOTS
  # ---------------------------------------------------------
  # We use show = FALSE to suppress the messy optimization text in the console
  fit_stat <- gev.fit(stn_data$MaxFlow, show = FALSE)
  
  # Set up an outer margin to force a master title onto the ismev plot grid
  par(oma = c(0, 0, 3, 0)) 
  
  # Generate the 4 diagnostic plots
  gev.diag(fit_stat)
  
  # Stamp the Station Name at the very top of the diagnostic plots
  mtext(paste("Station:", station_name, "- GEV Diagnostic Plots"), 
        outer = TRUE, cex = 1.5, font = 2)
  
  # Reset margins back to normal
  par(oma = c(0, 0, 0, 0))
  
  
  #extremes retrun level plot
  # Generate the return level plot
  # --- EXT-REMES RETURN LEVEL PLOT (As requested) ---
  # We fit silently with extRemes ONLY to utilize its return level plot engine
  #NEW PLOT single shows on a apge, no need for par(mfrow) or par(ask)
  par(mfrow = c(1, 1)) # Reset to single plot layout
  fit_ext <- fevd(stn_data$MaxFlow, type = "GEV")
  fit_ext$call <- NULL # Scrubs the text from the top of the plot
  plot(fit_ext, type = "rl", main = paste("Return Level Plot:", station_name))
  # ---------------------------------------------------------
  # ORDER 3: DECENT SUMMARY TABLE (Console Output)
  # ---------------------------------------------------------
  # Extract estimates and standard errors
  est <- fit_stat$mle
  se  <- fit_stat$se
  
  # Calculate Correlation Matrix
  corr_matrix <- cov2cor(fit_stat$cov)
  
  # Build a unified data frame for a clean console display
  summary_table <- data.frame(
    Parameter = c("Location (mu)", "Scale (sigma)", "Shape (xi)"),
    Estimate  = round(est, 4),
    Std_Error = round(se, 4),
    Corr_mu   = round(corr_matrix[, 1], 4),
    Corr_sig  = round(corr_matrix[, 2], 4),
    Corr_xi   = round(corr_matrix[, 3], 4)
  )
  
  cat("\n--- Parameter Estimates, Standard Errors, and Correlation Matrix ---\n")
  # Print the table cleanly without row numbers
  print(summary_table, row.names = FALSE)
  cat("--------------------------------------------------------------------\n\n")
  
  # Return the model silently in case you need to save it to a variable
  invisible(fit_stat)
}

# ==============================================================================
# HOW TO EXECUTE (OPTION A & B)
# ==============================================================================

# Get the list of the first 10 stations
target_stations <- unique(sf_hist$Station)[1:10]


# --- OPTION A: Run for ONE specific station ---
# To run the analysis for just one station, simply call the function with that station's name:
run_gev_analysis("FRSMS", sf_hist) # Replace 1 with

# --- OPTION B: Loop through the 10 stations ---
# If running the loop, R will draw plots very fast. You may want to use par(ask=TRUE) 
# so it prompts you to click before moving to the next station's plots.

# par(ask = TRUE) 
# for (stn in target_stations) {
#   run_gev_analysis(stn, sf_hist)
# }
# par(ask = FALSE)