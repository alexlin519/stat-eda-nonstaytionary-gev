rm(list = ls())
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)

bf_y <- rast("yearly_max_baseflow_History_1945_2012.tif")
pr_y <- rast("yearly_max_3day_prep_1945_2012.tif")

#1. convert directly to dataframes, keeping coordinates and dropping NAs
df_bf <- as.data.frame(bf_y, xy = TRUE, na.rm = TRUE)
df_pr <- as.data.frame(pr_y, xy = TRUE, na.rm = TRUE)

#2. pivot baseflow to long format and add a reliable time index per grid cell
df_bf_long <- df_bf %>%
  pivot_longer(cols = -c(x, y), names_to = "layer_name", values_to = "Baseflow") %>%
  group_by(x, y) %>%
  mutate(time_index = row_number()) %>%
  ungroup()

#3. pivot prep to long format and add the exact same time index
df_pr_long <- df_pr %>%
  pivot_longer(cols = -c(x, y), names_to = "layer_name", values_to = "Precipitation") %>%
  group_by(x, y) %>%
  mutate(time_index = row_number()) %>%
  ungroup()

#4. merge them perfectly by coordinates and time index
df_merged <- inner_join(
  df_bf_long %>% select(x, y, time_index, Baseflow),
  df_pr_long %>% select(x, y, time_index, Precipitation),
  by = c("x", "y", "time_index")
)

#5. scatter plot
plot_scatter <- ggplot(df_merged, aes(x = Baseflow, y = Precipitation)) +
  geom_point(alpha = 0.6, size = 1.5, color = "dodgerblue4") +
  theme_minimal() +
  labs(
    title = "Baseflow vs Precipitation",
    x = "Modeled Maximum Baseflow",
    y = "Maximum Precipitation"
  )

print(plot_scatter)


#normalize the data for better visualization
df_merged <- df_merged %>%
  mutate(
    Baseflow_norm = (Baseflow - min(Baseflow)) / (max(Baseflow) - min(Baseflow)),
    Precipitation_norm = (Precipitation - min(Precipitation)) / (max(Precipitation) - min(Precipitation))
  )
#plot the scatter plot with normalized values
plot_scatter_norm <- ggplot(df_merged, aes(x = Baseflow_norm, y = Precipitation_norm)) +
  geom_point(alpha = 0.6, size = 1.5, color = "coral3") +
  theme_minimal() +
  #add y=x
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Normalized Baseflow vs Normalized Precipitation",
    x = "Normalized Modeled Maximum Baseflow",
    y = "Normalized Maximum Precipitation"
  )
print(plot_scatter_norm)


# do teh same for monthly data
bf_m <- rast("monthly_max_baseflow_History_1945_2012.tif")
pr_m <- rast("monthly_max_3day_prep_1945_2012.tif")

df_bf_m <- as.data.frame(bf_m, xy = TRUE, na.rm = TRUE)
df_pr_m <- as.data.frame(pr_m, xy = TRUE, na.rm = TRUE)

df_bf_m_long <- df_bf_m %>%
  pivot_longer(cols = -c(x, y), names_to = "layer_name", values_to = "Baseflow") %>%
  group_by(x, y) %>%
  mutate(time_index = row_number()) %>%
  ungroup()

df_pr_m_long <- df_pr_m %>%
  pivot_longer(cols = -c(x, y), names_to = "layer_name", values_to = "Precipitation") %>%
  group_by(x, y) %>%
  mutate(time_index = row_number()) %>%
  ungroup()

df_merged_m <- inner_join(
  df_bf_m_long %>% select(x, y, time_index, Baseflow),
  df_pr_m_long %>% select(x, y, time_index, Precipitation),
  by = c("x", "y", "time_index")
)

plot_scatter_m <- ggplot(df_merged_m, aes(x = Baseflow, y = Precipitation)) +
  geom_point(alpha = 0.6, size = 1.5, color = "seagreen4") +
  theme_minimal() +
  labs(
    title = "Monthly Baseflow vs Monthly Precipitation",
    x = "Modeled Maximum Monthly Baseflow",
    y = "Maximum Monthly Precipitation"
  )
print(plot_scatter_m)

#normalized
df_merged_m <- df_merged_m %>%
  mutate(
    Baseflow_norm = (Baseflow - min(Baseflow)) / (max(Baseflow) - min(Baseflow)),
    Precipitation_norm = (Precipitation - min(Precipitation)) / (max(Precipitation) - min(Precipitation))
  )
plot_scatter_m_norm <- ggplot(df_merged_m, aes(x = Baseflow_norm, y = Precipitation_norm)) +
  geom_point(alpha = 0.6, size = 1.5, color = "orchid4") +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Normalized Monthly Baseflow vs Normalized Monthly Precipitation",
    x = "Normalized Modeled Maximum Monthly Baseflow",
    y = "Normalized Maximum Monthly Precipitation"
  )
print(plot_scatter_m_norm)














rm(list = ls())
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# 1. LOAD RASTERS (Baseflow, Prep, and the Date/DOY raster)
bf_y <- rast("yearly_max_baseflow_History_1945_2012.tif")
pr_y <- rast("yearly_max_3day_prep_1945_2012.tif")
# This is the layer that tells us WHICH day the max happened
doy_y <- rast("yearly_max_doy_History_1945_2012.tif") 

# 2. LOAD OBSERVED STREAMFLOW TABLE
# (Assuming your table from the picture is saved as a CSV)
st_table <- read.csv("stream_coor_station_hist.csv") 
st_table$Date <- as.Date(st_table$Date)

# 3. CONVERT RASTERS TO LONG DATA
# Function to flatten rasters and keep coordinates
get_long_df <- function(r, val_name) {
  as.data.frame(r, xy = TRUE, na.rm = TRUE) %>%
    pivot_longer(cols = -c(x, y), names_to = "Year", values_to = val_name) %>%
    mutate(Year = as.numeric(gsub("[^0-9]", "", Year)))
}

df_bf <- get_long_df(bf_y, "Baseflow")
df_pr <- get_long_df(pr_y, "Precipitation")
df_doy <- get_long_df(doy_y, "DOY")

# 4. MERGE RASTER DATA AND CALCULATE REAL DATES
df_raster <- df_bf %>%
  inner_join(df_pr, by = c("x", "y", "Year")) %>%
  inner_join(df_doy, by = c("x", "y", "Year")) %>%
  # Convert Year + DOY into a real Date object
  mutate(Date = as.Date(paste(Year, "01-01", sep="-")) + DOY - 1)

# 5. SPATIAL MATCHING (Grid to Station)
# We round coordinates to 3 decimal places to ensure they "snap" together
df_raster <- df_raster %>%
  mutate(x_round = round(x, 3), y_round = round(y, 3))

st_table <- st_table %>%
  mutate(x_round = round(Longitude, 3), y_round = round(Latitude, 3))

# 6. JOIN RASTER DATA WITH OBSERVED STREAMFLOW
# We join by the rounded coordinates AND the specific Date
df_final <- left_join(df_raster, 
                      st_table %>% select(x_round, y_round, Date, PNWNAmet), 
                      by = c("x_round", "y_round", "Date"))

# 7. SCATTER PLOT
# Points where we have a station match will be colored; 
# grids without a station will be grey/transparent.
ggplot(df_final, aes(x = Baseflow, y = Precipitation, color = PNWNAmet)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Yearly Extreme Baseflow vs. Precipitation",
    subtitle = "Colored by observed streamflow on the date of maximum baseflow",
    x = "Modeled Maximum Baseflow",
    y = "3-Day Precipitation Max",
    color = "Observed\nStreamflow"
  )

#