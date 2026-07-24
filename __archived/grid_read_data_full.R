library(terra)

# 1. Setup and Memory Management
terraOptions(memmax = 0.4, progress = 10) 

nc_path <- "Gridded 1945-2012 Baseflow.nc"
bf_stack <- rast(nc_path)["BASEFLOW"]

# Get unique years
all_years <- unique(format(time(bf_stack), "%Y"))
year_groups <- split(all_years, ceiling(seq_along(all_years) / 10))

# Result storage
monthly_max_list <- list()
yearly_max_list <- list()

print("Starting Optimized Incremental Processing...")

for (i in seq_along(year_groups)) {
  current_years <- year_groups[[i]]
  print(paste0("Processing Group ", i, "/", length(year_groups), 
               " (Years: ", min(current_years), "-", max(current_years), ")"))
  
  # 1. Subset Raw Data
  sub_stack <- bf_stack[[format(time(bf_stack), "%Y") %in% current_years]]
  
  # ---------------------------------------------------------
  # STEP A: Calculate Monthly Max (The Heavy Lift)
  # ---------------------------------------------------------
  # Create index YYYY-MM
  idx_monthly <- format(time(sub_stack), "%Y-%m")
  
  # Tapp 1: Raw -> Monthly
  chunk_monthly <- tapp(sub_stack, index = idx_monthly, fun = max, na.rm = TRUE)
  monthly_max_list[[i]] <- chunk_monthly
  
  # ---------------------------------------------------------
  # STEP B: Calculate Yearly Max (The Shortcut)
  # ---------------------------------------------------------
  # We derive Yearly Max from the Monthly chunk we just created.
  # This is incredibly fast because it processes 12 layers per year 
  # instead of 365 layers per year.
  
  # Extract year ("1945") from the monthly names/indices ("X1945.01" or similar)
  # We use the unique index from Step A to ensure alignment
  unique_months <- unique(idx_monthly)
  idx_yearly_from_month <- substr(unique_months, 1, 4)
  
  # Tapp 2: Monthly -> Yearly
  chunk_yearly <- tapp(chunk_monthly, index = idx_yearly_from_month, fun = max, na.rm = TRUE)
  yearly_max_list[[i]] <- chunk_yearly
  
  # ---------------------------------------------------------
  
  # Clean up
  rm(sub_stack, chunk_monthly, chunk_yearly)
  gc() 
}

print("Loop finished. Combining and Saving...")






# --- 1. Process Monthly Max Final ---
final_monthly_max <- rast(monthly_max_list)
# Fix names (remove 'X' prefix if terra added it, convert to YYYY.MM)
# Note: terra often converts "1945-01" to "X1945.01" in names
writeRaster(final_monthly_max, "monthly_max_1945_2012.tif", overwrite = TRUE)
print("Saved: monthly_max_1945_2012.tif")

# --- 2. Process Yearly Max Final ---
final_yearly_max <- rast(yearly_max_list)

# Verify layer count matches year count
if (nlyr(final_yearly_max) == length(all_years)) {
  names(final_yearly_max) <- all_years
} else {
  warning("Layer count mismatch on yearly stack.")
}

writeRaster(final_yearly_max, "yearly_max_1945_2012.tif", overwrite = TRUE)
print("Saved: yearly_max_1945_2012.tif")

# --- Verification Plot ---
par(mfrow=c(1,2))
plot(final_monthly_max[[1]], main = "First Month Max")
plot(final_yearly_max[[1]], main = "First Year Max")

#check the saved file, make sure there are 68 layers (1945-2012)
print(final_yearly_max)
print(res(final_yearly_max))
#The variable is BASEFLOW from a NetCDF file, representing the annual maximum 
#for each pixel from 1945–2012. The unit is mm/day. The region is Vancouver/PNW.
#check tail
#final_yearly_max	SpatRaster with 68 layers (Years 1945-2012).
#final_monthly_max	SpatRaster with 816 layers (68 years x 12 months).
print(tail(names(final_yearly_max), 10))


# Check units of all layers
units(bf_stack)

# Check if there's a specific unit for the first layer
units(bf_stack[[1]])


# 1. Check the resolution (pixel size)
# 检查分辨率（像素大小）
print("Resolution (x, y):")
print(res(bf_stack))

# 2. Check the coordinate reference system (units)
# 检查坐标参考系统（单位）
print("Coordinate System and Units:")
print(crs(bf_stack, describe = TRUE))
# The value 0.0625 confirms that your data is recorded on a 1/16th degree grid ($1 \div 16 = 0.0625$).
# 
# 值 0.0625 证实您的数据记录在 1/16 度网格上（ $1 \div 16 = 0.0625$ ）。
# 
# Since your CRS is WGS 84, this means each pixel is exactly $0.0625^{\circ}$ wide and $0.0625^{\circ}$ high.
# 
# 由于您的 CRS 为 WGS 84，这意味着每个像素的宽度正好是 $0.0625^{\circ}$ ，高度也是 $0.0625^{\circ}$ 。
# 
# What this looks like on the ground in Vancouver, using cellSize
# 
# 这在温哥华地面上的样子
# 
# Because Vancouver is near 49.28° N, the physical size of these "0.0625-degree squares" is not a perfect square in kilometers:
#   
#   由于温哥华位于北纬 49.28°附近，这些"0.0625 度正方形"的物理尺寸在公里上并不是一个完美的正方形：
# 
# Height (Latitude): This is constant everywhere.
# 
# $0.0625 \times 111 \text{ km} \approx \mathbf{6.94 \text{ km}}$
#   
#   高度（纬度）：在所有地方都是恒定的。 $0.0625 \times 111 \text{ km} \approx \mathbf{6.94 \text{ km}}$ 
#   
#   Width (Longitude): This shrinks as you go North.
# 
# $0.0625 \times 111 \text{ km} \times \cos(49.28^{\circ}) \approx \mathbf{4.53 \text{ km}}$
#   
#   宽度（经度）：向北移动时会缩小。 $0.0625 \times 111 \text{ km} \times \cos(49.28^{\circ}) \approx \mathbf{4.53 \text{ km}}$ 
#   
#   Calculation of Total Area
# 
# 总面积计算
# 
# Each of your pixels in the Vancouver region covers approximately 31.4 km² ($6.94 \times 4.53$).
# 
# 您在温哥华地区的每个像素覆盖的面积大约是 31.4 平方公里（ $6.94 \times 4.53$ ）






# 1. Total number of pixels in the rectangular grid
# 矩形网格中的总像素数
total_cells <- ncell(bf_stack)

# 2. Number of valid data points (excluding NAs/Ocean)
# 有效数据点的数量（排除 NA/海洋）
# We check the first layer (one day) and count non-NA values
valid_cells <- global(!is.na(bf_stack[[1]]), fun="sum")

print(paste("Total grid cells in box:", total_cells))
print(paste("Actual data points (Land/Study Area):", valid_cells))
#35,041 is the count of pixels that actually contain a number for Baseflow.

