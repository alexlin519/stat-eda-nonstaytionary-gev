
rm(list = ls())

library(terra)
library(ggplot2)
library(tidyr)

# --- 1. YEARLY DATA ---
bf_y <- rast("yearly_max_baseflow_History_1945_2012.tif")
pr_y <- rast("filtered_station_monthly_max_3day_prep_1945_2012.tif")
st_y <- rast("yearly_stream_History_1945_2012.tif")


#check st_y to confirm it has values only at station pixels
plot(st_y, main = "Streamflow Raster (Yearly)")
#plot on a map to confirm station locations
plot(st_y[[1]], main = "Streamflow Raster (Yearly) - Layer 1")

# THE FIX: Anchor the coordinates to the streamflow raster to get the exact station pixel
valid_st_cells <- which(!is.na(values(st_y[[1]])))
xy_st_coords <- xyFromCell(st_y, valid_st_cells)

# Extract directly to vectors using only the station pixels
bf_y_vec <- as.vector(values(bf_y)[valid_st_cells, ])
st_y_vec <- as.vector(values(st_y)[valid_st_cells, ])
pr_y_ext <- terra::extract(pr_y, xy_st_coords)[, -1]
pr_y_vec <- as.vector(as.matrix(pr_y_ext))

# Keep the safety net just in case the year count is off by 1
min_len_y <- min(length(bf_y_vec), length(st_y_vec), length(pr_y_vec))

df_yearly_final <- data.frame(
  Baseflow = bf_y_vec[1:min_len_y],
  Precipitation = pr_y_vec[1:min_len_y],
  Streamflow = st_y_vec[1:min_len_y]
)

df_yearly_clean <- drop_na(df_yearly_final)

plot_yearly <- ggplot(df_yearly_clean, aes(x = Baseflow, y = Precipitation, color = Streamflow)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(
    title = "Yearly Extreme Baseflow vs. 3-Day Precipitation (Station Pixel Only)",
    x = "Modeled Maximum Baseflow",
    y = "Maximum 3-Day Precipitation",
    color = "Observed Streamflow"
  )

print(plot_yearly)

# --- 2. MONTHLY DATA ---
bf_m <- rast("monthly_max_baseflow_History_1945_2012.tif")
pr_m <- rast("monthly_max_3day_prep_1945_2012.tif")
st_m <- rast("monthly_stream_History_1945_2012.tif")

# Anchor to streamflow for monthly data as well
valid_st_m_cells <- which(!is.na(values(st_m[[1]])))
xy_st_m_coords <- xyFromCell(st_m, valid_st_m_cells)

bf_m_vec <- as.vector(values(bf_m)[valid_st_m_cells, ])
st_m_vec <- as.vector(values(st_m)[valid_st_m_cells, ])
pr_m_ext <- terra::extract(pr_m, xy_st_m_coords)[, -1]
pr_m_vec <- as.vector(as.matrix(pr_m_ext))

min_len_m <- min(length(bf_m_vec), length(st_m_vec), length(pr_m_vec))

df_monthly_final <- data.frame(
  Baseflow = bf_m_vec[1:min_len_m],
  Precipitation = pr_m_vec[1:min_len_m],
  Streamflow = st_m_vec[1:min_len_m]
)

df_monthly_clean <- drop_na(df_monthly_final)

plot_monthly <- ggplot(df_monthly_clean, aes(x = Baseflow, y = Precipitation, color = Streamflow)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Monthly Extreme Baseflow vs. 3-Day Precipitation (Station Pixel Only)",
    x = "Modeled Maximum Baseflow",
    y = "Maximum 3-Day Precipitation",
    color = "Observed Streamflow"
  )

print(plot_monthly)












# --- 1. YEARLY DATA ---
bf_y <- rast("yearly_max_baseflow_History_1945_2012.tif")
pr_y <- rast("yearly_max_3day_prep_1945_2012.tif")
st_y <- rast("yearly_stream_History_1945_2012.tif")

valid_st_cells <- which(!is.na(values(st_y[[1]])))
xy_st_coords <- xyFromCell(st_y, valid_st_cells)

bf_y_vec <- as.vector(values(bf_y)[valid_st_cells, ])
pr_y_ext <- terra::extract(pr_y, xy_st_coords)[, -1]
pr_y_vec <- as.vector(as.matrix(pr_y_ext))

min_len_y <- min(length(bf_y_vec), length(pr_y_vec))

df_yearly_final <- data.frame(
  Baseflow = bf_y_vec[1:min_len_y],
  Precipitation = pr_y_vec[1:min_len_y]
)

df_yearly_clean <- drop_na(df_yearly_final)

plot_yearly <- ggplot(df_yearly_clean, aes(x = Baseflow, y = Precipitation)) +
  geom_point(alpha = 0.7, size = 2, color = "dodgerblue4") +
  theme_minimal() +
  labs(
    title = "Yearly Extreme Baseflow vs. 3-Day Precipitation",
    x = "Modeled Maximum Baseflow",
    y = "Maximum 3-Day Precipitation"
  )

print(plot_yearly)

# --- 2. MONTHLY DATA ---
bf_m <- rast("monthly_max_baseflow_History_1945_2012.tif")
pr_m <- rast("monthly_max_3day_prep_1945_2012.tif")
st_m <- rast("monthly_stream_History_1945_2012.tif")

valid_st_m_cells <- which(!is.na(values(st_m[[1]])))
xy_st_m_coords <- xyFromCell(st_m, valid_st_m_cells)

bf_m_vec <- as.vector(values(bf_m)[valid_st_m_cells, ])
pr_m_ext <- terra::extract(pr_m, xy_st_m_coords)[, -1]
pr_m_vec <- as.vector(as.matrix(pr_m_ext))

min_len_m <- min(length(bf_m_vec), length(pr_m_vec))

df_monthly_final <- data.frame(
  Baseflow = bf_m_vec[1:min_len_m],
  Precipitation = pr_m_vec[1:min_len_m]
)

df_monthly_clean <- drop_na(df_monthly_final)

plot_monthly <- ggplot(df_monthly_clean, aes(x = Baseflow, y = Precipitation)) +
  geom_point(alpha = 0.6, size = 1.5, color = "firebrick") +
  theme_minimal() +
  labs(
    title = "Monthly Extreme Baseflow vs. 3-Day Precipitation",
    x = "Modeled Maximum Baseflow",
    y = "Maximum 3-Day Precipitation"
  )

print(plot_monthly)



valid_st_cells <- which(!is.na(values(st_y[[1]])))
xy_coords_y <- xyFromCell(st_y, valid_st_cells)
min_layers_y <- min(nlyr(bf_y), nlyr(pr_y))
bf_y_synced <- bf_y[[1:min_layers_y]]
pr_y_synced <- pr_y[[1:min_layers_y]]
bf_y_ext <- terra::extract(bf_y_synced, xy_coords_y)[, -1]
pr_y_ext <- terra::extract(pr_y_synced, xy_coords_y)[, -1]
df_yearly <- data.frame(
  Baseflow = as.vector(as.matrix(bf_y_ext)),
  Precipitation = as.vector(as.matrix(pr_y_ext))
)

df_yearly_clean <- drop_na(df_yearly)

plot_yearly <- ggplot(df_yearly_clean, aes(x = Baseflow, y = Precipitation)) +
  geom_point(alpha = 0.7, size = 2, color = "dodgerblue4") +
  theme_minimal() +
  labs(
    title = "Yearly Extreme Baseflow vs. 3-Day Precipitation",
    x = "Modeled Maximum Baseflow",
    y = "Maximum 3-Day Precipitation"
  )

print(plot_yearly)


#do the same for monthly data
