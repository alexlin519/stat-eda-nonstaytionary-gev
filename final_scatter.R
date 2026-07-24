# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
# install.packages("patchwork") # Uncomment if you don't have it installed
library(patchwork)
library(scales) # For better axis labels
# --- SET YOUR TARGET DATASET HERE ---
target_dataset <- "rcp45"  # Change to "history", "rcp45", or "rcp85"

# ==========================================
# STEP 1: DYNAMICALLY LOAD THE DATA
# ==========================================
# Handle the slightly different naming conventions for the history files
if (target_dataset == "history") {
  pr_filename <- "year_history_sf_pr.csv"
  bf_filename <- "year_hist_sf_bf.csv"
} else {
  pr_filename <- paste0("year_", target_dataset, "_sf_pr.csv")
  bf_filename <- paste0("year_", target_dataset, "_sf_bf.csv")
}

print(paste("Loading Precipitation data from:", pr_filename))
pr_data <- read_csv(pr_filename)

print(paste("Loading Baseflow data from:", bf_filename))
bf_data <- read_csv(bf_filename)

# ==========================================
# STEP 2: SAFELY MERGE THE DATA
# ==========================================
# By listing ALL shared columns in the 'by' argument, R acts as a double-checker. 
# It will only merge rows where Station, Date, AND MaxFlow are exactly identical.
merged_data <- pr_data %>%
  inner_join(
    bf_data, 
    by = c("Station", "Longitude", "Latitude", "Year", "MaxDate", "MaxFlow")
  )

# Quick validation check: 
# If the row counts match, your streamflow events aligned perfectly!
cat("Rows in Precipitation Data:", nrow(pr_data), "\n")
cat("Rows in Baseflow Data:", nrow(bf_data), "\n")
cat("Rows in Merged Data:", nrow(merged_data), "\n")

# ==========================================
# STEP 3: CREATE THE SCATTER PLOTS
# ==========================================

# Clean the merged dataset by removing years
# and dropping any other stray NAs
clean_merged_data <- merged_data %>%
  tidyr::drop_na(MaxFlow, Prep_Covariate, Baseflow_Covariate)

# Verify the cleanup worked (this should print 0)
cat("Missing values left:", sum(is.na(clean_merged_data)), "\n")

# Plot 1: Y = MaxFlow, X = Prep, Color = Baseflow
plot1 <- ggplot(clean_merged_data, aes(x = Prep_Covariate, y = MaxFlow, color = Baseflow_Covariate)) +
  geom_point(alpha = 0.7, size = 2) + # alpha makes overlapping dots slightly transparent
  scale_color_viridis_c(option = "magma") + # A beautiful, colorblind-friendly continuous color scale
  theme_minimal() +
  labs(
    title = "Max Streamflow vs. Precipitation",
    x = "Precipitation Covariate (Smoothed)",
    y = "Yearly Maximum Streamflow",
    color = "Baseflow\nCovariate"
  )

# Plot 2: Y = MaxFlow, X = Baseflow, Color = Prep
plot2 <- ggplot(clean_merged_data, aes(x = Baseflow_Covariate, y = MaxFlow, color = Prep_Covariate)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_c(option = "viridis") + 
  theme_minimal() +
  labs(
    title = "Max Streamflow vs. Baseflow",
    x = "Baseflow Covariate",
    y = "Yearly Maximum Streamflow",
    color = "Precipitation\nCovariate"
  )

# ==========================================
# STEP 4: DISPLAY PLOTS
# ==========================================

# Display side-by-side using patchwork
combined_plot <- plot1 + plot2

# Print it to your viewer
combined_plot

# Optional: Save the plot as a high-quality image file
# ggsave("covariate_scatter_plots.png", combined_plot, width = 12, height = 5, dpi = 300)

# Optional: Save the final merged dataframe for your GEV models
# write_csv(merged_data, "year_history_sf_pr_bf_combined.csv")



# ==========================================
# STEP 5: Log Scales + Smoothers
# ==========================================
# Optio n 1: Log Scales + Smoothers
ggplot(clean_merged_data, aes(x = Prep_Covariate, y = MaxFlow, color = Baseflow_Covariate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam", color = "red", se = FALSE) + # Adds a non-linear trend line
  scale_x_log10(labels = label_number()) + # Log-transform the X axis
  scale_y_log10() + # Log-transform the Y axis
  scale_color_viridis_c(option = "magma") +
  theme_minimal() +
  labs(
    title = "Log-Scaled Streamflow vs. Precipitation",
    x = "Precipitation (Log Scale)",
    y = "Max Streamflow (Log Scale)"
  )


#same for prep vs maxflow but color by baseflow
ggplot(clean_merged_data, aes(x = Baseflow_Covariate, y = MaxFlow, color = Prep_Covariate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam", color = "blue", se = FALSE) + # Adds a non-linear trend line
  scale_x_log10(labels = label_number()) + # Log-transform the X axis
  scale_y_log10() + # Log-transform the Y axis
  scale_color_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(
    title = "Log-Scaled Streamflow vs. Baseflow",
    x = "Baseflow (Log Scale)",
    y = "Max Streamflow (Log Scale)"
  )
# Log-Scaled Streamflow vs. Baseflow
# filte rthe baseflow value to check the low outliers
ggplot(clean_merged_data %>% filter(Baseflow_Covariate < 10), aes(x = Baseflow_Covariate, y = MaxFlow, color = Prep_Covariate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam", color = "blue", se = FALSE) + # Adds a non-linear trend line
  scale_x_log10(labels = label_number()) + # Log-transform the X axis
  scale_y_log10() + # Log-transform the Y axis
  scale_color_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(
    title = "Log-Scaled Streamflow vs. Baseflow (Filtered for Low Baseflow)",
    x = "Baseflow (Log Scale)",
    y = "Max Streamflow (Log Scale)"
  )




# =========================================================================
# STEP 6: STATION-LEVEL SCATTER PLOTS (4 VARIATIONS)
# =========================================================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(patchwork)

# Select 9 random stations to look at closely
sample_stations <- unique(clean_merged_data$Station)[1:16] 

# Filter the data once to save processing time
plot_data <- clean_merged_data %>% filter(Station %in% sample_stations)

# -------------------------------------------------------------------------
# PLOT 1: Streamflow vs Baseflow | Colored by Precip | SHARED LEGEND
# -------------------------------------------------------------------------
plot1_shared <- ggplot(plot_data, aes(x = Baseflow_Covariate, y = MaxFlow, color = Prep_Covariate)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~Station, scales = "free") +
  theme_minimal() +
  labs(
    title = "Plot 1: MaxFlow vs Baseflow (Shared Precip Legend)",
    x = "Baseflow Covariate",
    y = "Max Streamflow",
    color = "Precipitation"
  )

# Display Plot 1
plot1_shared

# -------------------------------------------------------------------------
# PLOT 2: Streamflow vs Baseflow | Colored by Precip | INDEPENDENT LEGENDS
# -------------------------------------------------------------------------
# Function to build individual Baseflow plots
build_baseflow_plot <- function(station_name) {
  plot_data %>%
    filter(Station == station_name) %>%
    ggplot(aes(x = Baseflow_Covariate, y = MaxFlow, color = Prep_Covariate)) +
    geom_point(alpha = 0.7, size = 2) +
    scale_color_viridis_c(option = "viridis") +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      axis.title = element_text(size = 9)
    ) +
    labs(
      title = station_name,
      x = "Baseflow",
      y = "Max Flow",
      color = "Precip"
    )
}

# Generate and stitch Plot 2
plot_list_2 <- lapply(sample_stations, build_baseflow_plot)
plot2_independent <- wrap_plots(plot_list_2, ncol = 4) +
  plot_annotation(title = "Plot 2: MaxFlow vs Baseflow (Independent Precip Legends)")

# Display Plot 2
plot2_independent

# -------------------------------------------------------------------------
# PLOT 3: Streamflow vs Precip | Colored by Baseflow | SHARED LEGEND
# -------------------------------------------------------------------------
plot3_shared <- ggplot(plot_data, aes(x = Prep_Covariate, y = MaxFlow, color = Baseflow_Covariate)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_c(option = "magma") +
  facet_wrap(~Station, scales = "free") + 
  theme_minimal() +
  labs(
    title = "Plot 3: MaxFlow vs Precipitation (Shared Baseflow Legend)",
    x = "Precipitation Covariate",
    y = "Max Streamflow",
    color = "Baseflow"
  )

# Display Plot 3
plot3_shared

# -------------------------------------------------------------------------
# PLOT 4: Streamflow vs Precip | Colored by Baseflow | INDEPENDENT LEGENDS
# -------------------------------------------------------------------------
# Function to build individual Precipitation plots
build_prep_plot <- function(station_name) {
  plot_data %>%
    filter(Station == station_name) %>%
    ggplot(aes(x = Prep_Covariate, y = MaxFlow, color = Baseflow_Covariate)) +
    geom_point(alpha = 0.7, size = 2) +
    scale_color_viridis_c(option = "magma") +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      axis.title = element_text(size = 9)
    ) +
    labs(
      title = station_name,
      x = "Precipitation",
      y = "Max Flow",
      color = "Baseflow"
    )
}

# Generate and stitch Plot 4
plot_list_4 <- lapply(sample_stations, build_prep_plot)
plot4_independent <- wrap_plots(plot_list_4, ncol = 4) +
  plot_annotation(title = "Plot 4: MaxFlow vs Precipitation (Independent Baseflow Legends)")

# Display Plot 4
plot4_independent

# ==========================================
# STEP 7: Marginal Histograms
# ==========================================
# Option 4: Scatter with Marginal Histograms
#install.packages("ggExtra")
library(ggExtra)

# First, create your base plot (let's use log scales)
p <- ggplot(clean_merged_data, aes(x = Baseflow_Covariate, y = MaxFlow)) +
  geom_point(alpha = 0.4, color = "#2c3e50") +
  scale_x_log10() + 
  scale_y_log10() +
  theme_minimal()+
  #add title
  labs(
    title = "Max Streamflow vs Baseflow with Marginal Histograms",
    x = "Baseflow Covariate (Log Scale)",
    y = "Max Streamflow (Log Scale)"
  )

# Now wrap it in ggMarginal
ggMarginal(p, type = "histogram", fill = "steelblue", color = "white")


library(ggExtra)

# 1. Build the main plot, adding Prep_Covariate as the point color
pp <- ggplot(clean_merged_data, aes(x = Baseflow_Covariate, y = MaxFlow, color = Prep_Covariate)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_c(option = "magma") + 
  scale_x_log10() + 
  scale_y_log10() +
  theme_minimal() +
  # Move legend to the bottom so ggMarginal doesn't accidentally cover it
  theme(legend.position = "bottom") +
  labs(
    x = "Baseflow Covariate (Log Scale)", 
    y = "Max Streamflow (Log Scale)", 
    color = "Precipitation",
    title = "Max Streamflow vs Baseflow with Marginal Histograms"
  )

# 2. Add the margins. 
# (The margins will only show the distribution of Baseflow and MaxFlow)
ggMarginal(pp, type = "histogram", fill = "gray80", color = "white")

# ==========================================
# STEP 8: Hex Binning
# ==========================================
# Option 3: Hex Binning
# install.packages("hexbin") # You may need to install this first
ggplot(clean_merged_data, aes(x = Baseflow_Covariate, y = MaxFlow)) +
  geom_hex(bins = 50) + # Creates 50 hex bins across the grid
  scale_fill_viridis_c(option = "plasma", trans = "log") + # Colors by point count
  theme_minimal() +
  labs(
    title = "Density of Extreme Events: Baseflow vs MaxFlow",
    fill = "Log(Count of Events)"
  )





# ==========================================
# Faceting by a third variable
# ==========================================

clean_merged_data %>%
  mutate(Baseflow_Bin = cut(Baseflow_Covariate, 
                            breaks = quantile(Baseflow_Covariate, 0:4/4, na.rm=TRUE),
                            labels = c("Low BF","Med-Low BF","Med-High BF","High BF"))) %>%
  ggplot(aes(x = Prep_Covariate, y = MaxFlow)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "gam", color = "red") +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~ Baseflow_Bin)
ggplot(clean_merged_data, aes(x = Baseflow_Covariate, y = MaxFlow)) +
  geom_point(alpha = 0.2) +
  geom_density_2d_filled(alpha = 0.5) +
  scale_x_log10(labels = label_number()) + scale_y_log10()

ggplot(clean_merged_data, aes(x = Baseflow_Covariate, y = MaxFlow)) +
  geom_point(alpha = 0.2) +
  geom_density_2d_filled(alpha = 0.5) +
  scale_x_log10(labels = label_number()) + scale_y_log10()



# 1. Quick Summary of NAs per column
cat("--- Missing Values per Column ---\n")
colSums(is.na(merged_data))

# 2. Isolate the problematic rows so you can look at them
missing_data_rows <- merged_data %>%
  filter(is.na(MaxFlow) | is.na(Prep_Covariate) | is.na(Baseflow_Covariate))

# View the first few rows of the missing data
head(missing_data_rows)

# 3. Figure out WHICH stations are missing the most data
missing_by_station <- missing_data_rows %>%
  group_by(Station) %>%
  summarize(
    Missing_Prep = sum(is.na(Prep_Covariate)),
    Missing_Baseflow = sum(is.na(Baseflow_Covariate)),
    Missing_MaxFlow = sum(is.na(MaxFlow)),
    Total_Missing_Rows = n()
  ) %>%
  arrange(desc(Total_Missing_Rows))

# View the top stations with missing data
head(missing_by_station, 10)



