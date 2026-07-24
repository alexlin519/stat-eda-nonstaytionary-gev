#read netpdf data
if (!requireNamespace("ncdf4", quietly = TRUE)) {
  install.packages("ncdf4")
}
if (!requireNamespace("raster", quietly = TRUE)) {
  install.packages("raster")
}
library(ncdf4)
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)


#check data path correct or not

#read from same director 
#need to change data bc now it is just one year
nc_data <- nc_open("Hydrologic Model Output 1945-2099.nc")



# section 1
print(nc_data)
# Extract variable names, dimensions, and attributes
attributes(nc_data$var)$names

# 1.5
#check the time dimension in the netcdf file
time_var <- ncvar_get(nc_data, "time")
time_units <- ncatt_get(nc_data, "time", "units")$value
time_calendar <- ncatt_get(nc_data, "time", "calendar")$value
print(time_units)
print(time_calendar)
# Convert time to Date format
time_origin <- as.Date(substr(time_units, 12, 21))
time_dates <- time_origin + time_var
print(head(time_dates))
time_dates




#section 2
# from the time date we know it is daily data
# Plot some random dates to visualize the data
hydrology_stack <- stack("Hydrologic Model Output 1945-2099.nc",
                        varname = "BASEFLOW") # replace with actual variable name

# Plot for a few selected dates
plot_dates <- c("1945-01-01", "1945-05-01","1945-07-01","1946-01-01")
for (date in plot_dates) {
  date_index <- which(time_dates == as.Date(date))
  if (length(date_index) > 0) {
    plot(hydrology_stack[[date_index]], main = paste("Hydrology on", date))
  }
}





#section 3
library(terra)

# 1. Load the entire dataset
nc_raw <- rast("Hydrologic Model Output 1945-2099.nc")

# 2. Check the variable names inside (to be sure)
print(names(nc_raw))

# 3. Extract ONLY the BASEFLOW variable
# (This acts like selecting a column in a dataframe)
bf_stack <- nc_raw["BASEFLOW"] 

# Check the result
print(bf_stack)



#section 4
#Validate the Geography
# Calculate the total baseflow for the entire year (sum across time)
total_baseflow <- app(bf_stack, sum, na.rm = TRUE)

# Plot with a map background
plot(total_baseflow, 
     main = "Total Baseflow Accumulation (1945)", 
     col = topo.colors(100),
     axes = TRUE)

#Check for Seasonality (Time Series)
# Calculate the average baseflow across the entire map for every day
# 'global' is a terra function for spatial statistics
daily_avg <- global(bf_stack, "mean", na.rm = TRUE)

# Create a clean data frame for plotting
df_trend <- data.frame(
  Date = time(bf_stack),
  Baseflow_mm = daily_avg$mean
)

# Plot
library(ggplot2)
ggplot(df_trend, aes(x = Date, y = Baseflow_mm)) +
  geom_line(color = "steelblue", size = 1) +
  theme_minimal() +
  labs(title = "Average Daily Baseflow (1945)",
       y = "Mean Baseflow (mm)",
       x = "Date")


# section 5
#Extract Data for a Specific Location
# 1. Get the first layer to check for valid cells
first_layer <- bf_stack[[1]]

# 2. Find all cells that are NOT NA
# "as.points" converts raster cells to vector points
valid_points <- as.points(first_layer, na.rm = TRUE)

# 3. Pick a random valid point (or just the first one)
# This guarantees we are on land!
sample_point <- valid_points[1000] # Picking the 1000th valid point to be safe
coords <- crds(sample_point)
#Extract the specific coordinates
valid_lon <- sample_point$x
valid_lat <- sample_point$y
print(paste("Found valid land at Lon:", coords[1], "Lat:", coords[2]))

# 4. Extract data for this GUARANTEED valid point
point_data <- terra::extract(bf_stack, sample_point, cells=FALSE, ID=FALSE)

# 5. Plot again
point_ts <- data.frame(
  Date = time(bf_stack),
  Baseflow = as.numeric(point_data[1, ])
)

ggplot(point_ts, aes(x = Date, y = Baseflow)) +
  geom_line(color = "darkgreen") +
  labs(title = paste("Baseflow at Valid Location (1945)"), 
       subtitle = paste("Lat:", round(coords[2], 2), "Lon:", round(coords[1], 2)))



#Histogram of Values (Sanity Check)
# Take a sample of 10,000 pixels to speed up plotting
samp <- spatSample(bf_stack, size = 10000, method = "regular", na.rm = TRUE)

# Convert to vector
vals <- as.vector(as.matrix(samp))

# Simple Histogram
hist(vals, 
     breaks = 50, 
     col = "orange", 
     main = "Distribution of Baseflow Values",
     xlab = "Baseflow (mm)")






# section 6
library(terra)

# 1. Calculate the 'Mean' and 'Standard Deviation' across all 366 layers
# 'app' applies a function to every pixel stack
mean_map <- app(bf_stack, mean, na.rm = TRUE)
sd_map   <- app(bf_stack, sd, na.rm = TRUE)

# 2. Plot them side-by-side
par(mfrow = c(1, 2)) # Split screen into 2 columns
plot(mean_map, main = "Average Baseflow (1945)", col = topo.colors(100))
plot(sd_map,   main = "Volatility (Std Dev)",    col = heat.colors(100))
par(mfrow = c(1, 1)) # Reset screen
#If a spot is Blue on the Mean map but Red on the SD map, 
#it receives a lot of water, but it comes in unpredictable bursts (storm-driven).
#"Which areas are consistently wet, and which areas fluctuate the most?"



# section 7
# Install ggridges if you don't have it
if (!requireNamespace("ggridges", quietly = TRUE)) install.packages("ggridges")
library(ggridges)

# Reuse the 'df_long' from the previous step
df_long %>%
  mutate(Month = format(Date, "%B")) %>%
  # Reorder months correctly
  mutate(Month = factor(Month, levels = month.name)) %>% 
  filter(Day_Index %% 5 == 0) %>% # Sample every 5th day to speed up plotting
  ggplot(aes(x = Baseflow, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "mm", option = "C") +
  labs(title = "Monthly Baseflow Distributions",
       x = "Baseflow (mm)") +
  theme_ridges()



#--------------

# section 8 

jan_data <- df_trend %>%
  filter(format(Date, "%B") == "January")

# 2. Define Threshold (e.g., 99th percentile)
# 定义阈值（例如：99百分位数）
threshold <- quantile(jan_data$Baseflow_mm, 0.99)
print(paste("99th Percentile Threshold:", round(threshold, 3), "mm"))

# 3. Identify Extreme Events
# 识别极端事件
extreme_events <- jan_data %>%
  filter(Baseflow_mm > threshold) %>%
  arrange(desc(Baseflow_mm))

# Display the top extreme dates
# 显示最极端的日期
library(knitr)
kable(extreme_events, caption = "Top Extreme Baseflow Events in January")

# 4. Visualization: Timeline of January Extremes
# 可视化：一月份极值的时间线
ggplot(jan_data, aes(x = Date, y = Baseflow_mm)) +
  geom_point(alpha = 0.5, color = "gray") +
  geom_point(data = extreme_events, color = "red", size = 2) +
  geom_text(data = extreme_events, aes(label = Date), 
            vjust = -1, size = 3, check_overlap = TRUE) +
  labs(title = "January Baseflow Anomalies",
       subtitle = "Red points indicate values above the 99th percentile",
       y = "Mean Baseflow (mm)") +
  theme_minimal()



# Get the date of the absolute maximum event
# 获取绝对最大事件的日期
max_event_date <- extreme_events$Date[1]
print(paste("Analyzing spatial pattern for:", max_event_date))

# Find the index in the raster stack
# 在栅格堆栈中找到索引
date_idx <- which(time(bf_stack) == max_event_date)

if(length(date_idx) > 0) {
  # Extract that specific layer
  # 提取该特定图层
  extreme_layer <- bf_stack[[date_idx]]
  
  # Plot with custom breaks to highlight hotspots
  # 使用自定义断点绘图以突出显示热点
  plot(extreme_layer, 
       main = paste("Spatial Distribution on", max_event_date),
       col = terra::map.pal("viridis", 100),
       range = c(0, max(values(extreme_layer), na.rm=TRUE)))
}

### 8.2 Statistical Check: Q-Q Plot (统计检查：Q-Q 图)

Since you are interested in extreme values, a Q-Q plot helps assess if the data follows a Normal distribution or a heavy-tailed distribution (like Gumbel/Frechet).
由于您对极值感兴趣，Q-Q 图有助于评估数据是服从正态分布还是厚尾分布（如 Gumbel/Frechet）。

```{r qq_plot}
# Normal Q-Q Plot for January
# 一月份的正态 Q-Q 图
ggplot(jan_data, aes(sample = Baseflow_mm)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot for January Baseflow",
       subtitle = "Deviation from the line indicates non-normality (heavy tails)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_light()

```

```

### Key Statistical Improvements (关键统计改进)

* **Quantile Filtering ($99^{th}$ Percentile):**
  Instead of guessing, we mathematically isolate the top $1\%$ of days.
我们没有猜测，而是从数学上分离出前 $1\%$ 的日子。

* **Spatial Context (Correct `terra` usage):**
  A high average could mean the whole map is slightly wet, or one small area is flooding massively. The spatial plot answers this.
高平均值可能意味着整个地图稍微潮湿，或者一个小区域正在发生大规模洪水。空间图回答了这个问题。

* **Q-Q Plot:**
  Given your background in Statistics, this visualizes the tail behavior $P(X > x)$ effectively. If the points curve upward away from the red line on the right side, it confirms a heavy-tailed distribution suitable for EVT (Extreme Value Theory) modeling.
考虑到您的统计学背景，这有效地可视化了尾部行为 $P(X > x)$。如果点在右侧向上偏离红线，则证实了适合 EVT（极值理论）建模的厚尾分布。

```

















# Get the actual values BEFORE leaflet processes them
all_values <- values(test_day)

# Create palette
pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), 
                    domain = actual_range,
                    na.color = "transparent")

# Map with explicit value passing
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(test_day, 
                 colors = pal, 
                 opacity = 0.8,
                 project = FALSE) %>%  # Try this!
  addLegend(pal = pal, 
            values = all_values,  # Use the actual values
            title = "Day 1 Baseflow")












library(terra)
library(leaflet)

# 1. Calculate the average flow for every day
daily_means <- global(bf_stack, "mean", na.rm = TRUE)

# 2. Find which day had the highest average flow
max_day_index <- which.max(daily_means$mean)
max_value <- max(daily_means$mean)

print(paste("The wettest day is Day number:", max_day_index))
print(paste("Average flow on that day:", round(max_value, 2), "mm"))

# 3. Select that specific layer
wettest_layer <- bf_stack[[max_day_index]]

# 4. Get the actual values BEFORE leaflet processes them
all_values <- values(wettest_layer)

# 5. Get the actual range
actual_range <- range(all_values, na.rm = TRUE)
print(paste("Data range:", actual_range[1], "to", actual_range[2]))

# 6. Create domain (starting at 0 to show water as blue)
domain_vals <- c(0, actual_range[2])

# 7. Create Palette
pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), 
                    domain = domain_vals,
                    na.color = "transparent")

# 8. Create the interactive map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(wettest_layer, 
                 colors = pal, 
                 opacity = 0.8,
                 project = FALSE,  # This was the key fix!
                 maxBytes = 20 * 1024 * 1024) %>%
  addLegend(pal = pal, 
            values = all_values,  # Use actual values
            title = paste("Baseflow Day", max_day_index))


# get the map for a specific date
specific_date <- as.Date("1945-07-15")
date_index <- which(time(bf_stack) == specific_date)

if(length(date_index) > 0) {
  specific_layer <- bf_stack[[date_index]]
  
  # Get actual values
  all_values <- values(specific_layer)
  
  # Get actual range
  actual_range <- range(all_values, na.rm = TRUE)
  print(paste("Data range:", actual_range[1], "to", actual_range[2]))
  
  # Create domain
  domain_vals <- c(0, actual_range[2])
  
  # Create palette
  pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), 
                      domain = domain_vals,
                      na.color = "transparent")
  
  # Create the interactive map
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addRasterImage(specific_layer, 
                   colors = pal, 
                   opacity = 0.8,
                   project = FALSE,
                   maxBytes = 20 * 1024 * 1024) %>%
    addLegend(pal = pal, 
              values = all_values, 
              title = paste("Baseflow on", specific_date))
} else {
  print("Date not found in the dataset.")
}



# Function to create map for any day
plot_baseflow_day <- function(day_number) {
  layer <- bf_stack[[day_number]]
  all_values <- values(layer)
  actual_range <- range(all_values, na.rm = TRUE)
  domain_vals <- c(0, actual_range[2])
  
  pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), 
                      domain = domain_vals,
                      na.color = "transparent")
  
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addRasterImage(layer, 
                   colors = pal, 
                   opacity = 0.8,
                   project = FALSE,
                   maxBytes = 20 * 1024 * 1024) %>%
    addLegend(pal = pal, 
              values = all_values,
              title = paste("Baseflow Day", day_number))
}

# Use it: plot any day you want
plot_baseflow_day(200)  # Day 100
plot_baseflow_day(max_day_index)  # Wettest day
