library(terra)
library(readxl)

# 1. Setup and Target Selection
# ---------------------------------------------------------
terraOptions(memmax = 0.8, progress = 10) # Increased memory allowance for the big grab

target_dataset <- "history"   # Options: "history", "rcp45", "rcp85"
target_threshold <- "95"      # Options: "90", "95", "99"
variable_name <- "bf"   # The variable name in the NetCDF files
prob_val <- as.numeric(target_threshold) / 100

# Define File Paths
file_history <- "netCDF_data/Gridded 1945-2012 Baseflow.nc"

files_rcp45 <- c(
  "netCDF_data/CanESM2 RCP45 Baseflow 1945-2099 1.nc",
  "netCDF_data/CanESM2 RCP45 Baseflow 1945-2099 3.nc", 
  "netCDF_data/CanESM2 RCP45 Baseflow 1945-2099 2.nc"
)

files_rcp85 <- c(
  "netCDF_data/CanESM2 RCP85 Baseflow 1945-2099 1.nc",
  "netCDF_data/CanESM2 RCP85 Baseflow 1945-2099 3.nc",
  "netCDF_data/CanESM2 RCP85 Baseflow 1945-2099 2.nc"
)
if (target_dataset == "history") {
  active_files <- file_history
} else if (target_dataset == "rcp45") {
  active_files <- files_rcp45
} else if (target_dataset == "rcp85") {
  active_files <- files_rcp85
} else {
  stop("Invalid target_dataset.")
}

# 2. Load Data and Crop to Station Area (MASSIVE SPEEDUP)
# ---------------------------------------------------------
print("Loading data and cropping to station extent...")
coords_df <- read_excel("station_coordinate_apr15.xlsx")
stations_pts <- vect(coords_df, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")

bf_stack <- rast(active_files)["BASEFLOW"]

# Create a bounding box around stations and add a 1-degree buffer so the 5x5 grids fit
stations_ext <- ext(stations_pts) + 1 

# Crop the massive stack. Now terra only has to think about this small region!
bf_cropped <- crop(bf_stack, stations_ext)
template_layer <- bf_cropped[[1]]

# 3. Calculate 5x5 Grid Boundaries on the Cropped Area
# ---------------------------------------------------------
print("Calculating 5x5 grid boundaries...")
center_cells <- cells(template_layer, stations_pts)[, "cell"]
ring_1 <- adjacent(template_layer, center_cells, directions = 8, include = TRUE)
ring_2 <- adjacent(template_layer, unique(as.vector(ring_1)), directions = 8, include = TRUE)

keep_cells <- unique(as.vector(ring_2)) 
keep_cells <- keep_cells[!is.na(keep_cells)]

# 4. The "One-Shot" Extraction and Calculation
# ---------------------------------------------------------
print(paste0("Extracting all 24,000+ days for ", length(keep_cells), " cells at once... (This may take a minute)"))

# Grab everything in one single read operation
# This creates a data.frame where rows = cells, columns = days
cell_data <- bf_cropped[keep_cells] 

# Drop the geometry/ID columns if terra includes them (we just want numeric baseflow)
# If the first column is the cell ID, we remove it for the calculation
if (names(cell_data)[1] == "cell") {
  cell_data <- cell_data[, -1]
}

print(paste0("Calculating ", target_threshold, "th percentile..."))
# Calculate the quantile for every row (cell) simultaneously
calculated_thresholds <- apply(cell_data, 1, quantile, probs = prob_val, na.rm = TRUE)


# 5. Map Back to Original Extent and Save
# ---------------------------------------------------------
print("Mapping thresholds back to spatial raster...")

# Create an empty raster using the cropped template
t_rast_cropped <- rast(template_layer, vals = NA)

# Insert the calculated thresholds
t_rast_cropped[keep_cells] <- calculated_thresholds

# Expand the raster back to the original massive NetCDF size 
# so it lines up perfectly perfectly with your other scripts
t_rast_final <- extend(t_rast_cropped, bf_stack[[1]])

#name it based on variable_name
output_filename <- paste0(variable_name, "_threshold_", target_threshold, "_", target_dataset, ".tif")
writeRaster(t_rast_final, output_filename, overwrite = TRUE)

print(paste("Success!", output_filename, "has been saved."))



#plot to check
plot(t_rast_final, main = paste0(variable_name, " ", target_threshold, "th Percentile - ", target_dataset))

# print as df
threshold_df <- as.data.frame(t_rast_final, xy = TRUE)
colnames(threshold_df) <- c("Longitude", "Latitude", "Threshold")
print(head(threshold_df))
