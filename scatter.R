


library(terra)
library(dplyr)
library(ggplot2)

# Assuming you have already run steps 1 through 4 from the previous code
# to create station_mapping and the extraction matrices (ext_prep_y_7d, etc.)

# 1. Calculate the number of time steps
n_years <- 68 # 1945 to 2012
n_months <- 68 * 12 # 816 months

# 2. Create vectors of repeating station names to match the flattened data
stations_yearly <- rep(station_mapping$station, times = n_years)
stations_monthly <- rep(station_mapping$station, times = n_months)

# 3. Rebuild the dataframes with the Station column included
df_yearly_stations <- data.frame(
  Station = stations_yearly,
  Prep_7Day_Max = as.vector(as.matrix(ext_prep_y_7d)),
  Baseflow_Max = as.vector(as.matrix(ext_bf_y))
) %>% na.omit()

df_monthly_stations <- data.frame(
  Station = stations_monthly,
  Prep_3Day_Max = as.vector(as.matrix(ext_prep_m_3d)),
  Prep_7Day_Max = as.vector(as.matrix(ext_prep_m_7d)),
  Baseflow_Max = as.vector(as.matrix(ext_bf_m))
) %>% na.omit()

# 4. Generate the scatter plots colored by Station
plot_m_3d_colored <- ggplot(df_monthly_stations, aes(x = Prep_3Day_Max, y = Baseflow_Max, color = Station)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  # Remove the legend if it takes up too much space, or keep it to identify colors
  theme(legend.position = "right", legend.key.size = unit(0.5, "cm")) +
  guides(color = guide_legend(ncol = 2)) + # Split legend into 2 columns for better fit
  labs(title = "Monthly Max Baseflow vs 3-Day Precip (Colored by Station)",
       x = "Monthly Max 3-Day Precipitation (mm)",
       y = "Monthly Max Baseflow")

plot_y_7d_colored <- ggplot(df_yearly_stations, aes(x = Prep_7Day_Max, y = Baseflow_Max, color = Station)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "right", legend.key.size = unit(0.5, "cm")) +
  guides(color = guide_legend(ncol = 2)) +
  labs(title = "Yearly Max Baseflow vs 7-Day Precip (Colored by Station)",
       x = "Yearly Max 7-Day Precipitation (mm)",
       y = "Yearly Max Baseflow")

plot_m_7d_colored <- ggplot(df_monthly_stations, aes(x = Prep_7Day_Max, y = Baseflow_Max, color = Station)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  theme(legend.position = "right", legend.key.size = unit(0.5, "cm")) +
  guides(color = guide_legend(ncol = 2)) +
  labs(title = "Monthly Max Baseflow vs 7-Day Precip (Colored by Station)",
       x = "Monthly Max 7-Day Precipitation (mm)",
       y = "Monthly Max Baseflow")

# Print the plots
print(plot_m_3d_colored)
print(plot_y_7d_colored)
print(plot_m_7d_colored)

