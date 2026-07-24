library(terra)
#read the tif files we just created
final_monthly_max <- rast("monthly_max_1945_2012.tif")
final_yearly_max <- rast("yearly_max_1945_2012.tif")

# ---------------------------------------------------------
# ---------------------------------------------------------
# ---------------------------------------------------------
# PART 2: GLOBAL TREND OF EXTREMES
# 第二部分：极端情况的全球趋势
# ---------------------------------------------------------
# ---------------------------------------------------------
# ---------------------------------------------------------
# ---------------------------------------------------------
# ---------------------------------------------------------



# PART 2: GLOBAL TREND OF EXTREMES
# 第二部分：极端情况的全球趋势
# ---------------------------------------------------------
# 1. Create area weight raster
pixel_areas <- cellSize(final_yearly_max[[1]], unit = "km") 

# 2. Calculate the global weighted mean
print("Calculating global averages with area weights...")
global_means <- global(final_yearly_max, fun = "mean", na.rm = TRUE, weights = pixel_areas)

# 3. Create the dataframe safely
# We use [, 1] to ensure we get the numeric values even if the column name changed
years <- as.numeric(names(final_yearly_max))
trend_df <- data.frame(
  Year = years, 
  MeanMaxBaseflow = global_means[, 1] 
)

# Double check the data
print(head(trend_df))

# 4. Plot the trend
plot(trend_df$Year, trend_df$MeanMaxBaseflow, 
     type = "o", pch = 19, col = "steelblue",
     xlab = "Year", ylab = "Weighted Mean Max Baseflow",
     main = "Global Trend of Extremes")
#PLOT not showing full, but half left, so make it full




# ---------------------------------------------------------
# PART 3: SPATIAL HOTSPOTS (WHERE IS THE WORST?)
# 第三部分：空间热点（哪里最严重？）
# ---------------------------------------------------------

# Find the absolute maximum value ever recorded for each pixel
# 找出每个像素记录的绝对最大值
# This maps the "Worst Case Scenario" from 1945-2012
# 这绘制了 1945-2012 年间的“最坏情况”
worst_case_map <- app(final_yearly_max, fun = max, na.rm = TRUE)

plot(worst_case_map, 
     main = "Absolute Maximum Baseflow Record (1945-2012)", 
     col = heat.colors(50, rev = TRUE))



  
  
  
  