library(terra)

# 1. Setup and Memory Management
terraOptions(memmax = 0.4, progress = 10) 

# ==============================================================================
# --- SET YOUR TARGET DATASET AND WINDOW HERE ---
# ==============================================================================
target_dataset <- "history" # Options: "history", "rcp45", "rcp85"
target_window  <- "3d"   # Options: "1d", "3d", "7d", "all" (calculates all three windows)
# ==============================================================================

file_history <- "netCDF_data/Gridded 1945-2012 PREC.nc"

files_rcp45 <- c(
  "netCDF_data/allwsbc.CanESM2_rcp45_r1i1p1.1945to2099.PREC.nc (2).nc",
  "netCDF_data/allwsbc.CanESM2_rcp45_r1i1p1.1945to2099.PREC.nc (1).nc", 
  "netCDF_data/allwsbc.CanESM2_rcp45_r1i1p1.1945to2099.PREC.nc.nc"      
)

files_rcp85 <- c(
  "netCDF_data/allwsbc.CanESM2_rcp85_r1i1p1.1945to2099.PREC.nc.nc",     
  "netCDF_data/allwsbc.CanESM2_rcp85_r1i1p1.1945to2099.PREC.nc (1).nc", 
  "netCDF_data/allwsbc.CanESM2_rcp85_r1i1p1.1945to2099.PREC.nc (2).nc"
)

process_optimized <- function(file_vector, scen_suffix, chunk_years = 10, window_type = "all") {
  print(paste("--- Starting FASTER Extraction for:", scen_suffix, " | Window:", window_type, "---"))
  
  pr_stack <- rast(file_vector)["PREC"]
  full_time <- time(pr_stack)
  full_years <- as.numeric(format(full_time, "%Y"))
  unique_years <- unique(full_years)
  
  year_groups <- split(unique_years, ceiling(seq_along(unique_years) / chunk_years))
  
  list_m1 <- list(); list_y1 <- list(); list_m1_day <- list(); list_y1_doy <- list()
  list_m3 <- list(); list_y3 <- list(); list_m3_day <- list(); list_y3_doy <- list()
  list_m7 <- list(); list_y7 <- list(); list_m7_day <- list(); list_y7_doy <- list()
  
  pad_before <- 0; pad_after <- 0
  if (window_type %in% c("3d", "7d", "all")) {
    pad_before <- 3; pad_after <- 3 # Use max required padding for simplicity
  }
  
  for (i in seq_along(year_groups)) {
    current_years <- year_groups[[i]]
    print(sprintf("Processing Block %d/%d (Years: %d - %d)", 
                  i, length(year_groups), min(current_years), max(current_years)))
    
    idx_current <- which(full_years %in% current_years)
    idx_needed <- idx_current
    
    if (pad_before > 0 && i > 1) {
      idx_needed <- c((idx_current[1] - pad_before):(idx_current[1] - 1), idx_needed)
    }
    if (pad_after > 0 && i < length(year_groups)) {
      idx_needed <- c(idx_needed, (idx_current[length(idx_current)] + 1):(idx_current[length(idx_current)] + pad_after))
    }
    
    sub_stack <- pr_stack[[idx_needed]]
    sub_time <- time(sub_stack)
    sub_n <- nlyr(sub_stack)
    
    # --- 1-Day Logic ---
    if (window_type %in% c("1d", "all")) {
      idx_1d <- which(as.numeric(format(sub_time, "%Y")) %in% current_years)
      pr_1d <- sub_stack[[idx_1d]]
      ym_1d <- format(time(pr_1d), "%Y-%m"); y_1d <- format(time(pr_1d), "%Y")
      
      list_m1[[i]] <- tapp(pr_1d, ym_1d, max, na.rm = TRUE)
      list_y1[[i]] <- tapp(pr_1d, y_1d, max, na.rm = TRUE) 
      list_m1_day[[i]] <- tapp(pr_1d, ym_1d, which.max)
      list_y1_doy[[i]] <- tapp(pr_1d, y_1d, which.max)
      rm(pr_1d)
    }
    
    # --- 3-Day Logic ---
    if (window_type %in% c("3d", "all")) {
      # Calculate rolling sum in-memory
      pr_3d_full <- sub_stack[[1:(sub_n-2)]] + sub_stack[[2:(sub_n-1)]] + sub_stack[[3:sub_n]]
      time(pr_3d_full) <- sub_time[2:(sub_n-1)] 
      
      idx_3d <- which(as.numeric(format(time(pr_3d_full), "%Y")) %in% current_years)
      pr_3d <- pr_3d_full[[idx_3d]]
      ym_3d <- format(time(pr_3d), "%Y-%m"); y_3d <- format(time(pr_3d), "%Y")
      
      list_m3[[i]] <- tapp(pr_3d, ym_3d, max, na.rm = TRUE)
      list_y3[[i]] <- tapp(pr_3d, y_3d, max, na.rm = TRUE)
      list_m3_day[[i]] <- tapp(pr_3d, ym_3d, which.max)
      list_y3_doy[[i]] <- tapp(pr_3d, y_3d, which.max)
      rm(pr_3d_full, pr_3d)
    }
    
    # --- 7-Day Logic ---
    if (window_type %in% c("7d", "all")) {
      pr_7d_full <- sub_stack[[1:(sub_n-6)]] + sub_stack[[2:(sub_n-5)]] + 
        sub_stack[[3:(sub_n-4)]] + sub_stack[[4:(sub_n-3)]] + 
        sub_stack[[5:(sub_n-2)]] + sub_stack[[6:(sub_n-1)]] + 
        sub_stack[[7:sub_n]]
      time(pr_7d_full) <- sub_time[4:(sub_n-3)] 
      
      idx_7d <- which(as.numeric(format(time(pr_7d_full), "%Y")) %in% current_years)
      pr_7d <- pr_7d_full[[idx_7d]]
      ym_7d <- format(time(pr_7d), "%Y-%m"); y_7d <- format(time(pr_7d), "%Y")
      
      list_m7[[i]] <- tapp(pr_7d, ym_7d, max, na.rm = TRUE)
      list_y7[[i]] <- tapp(pr_7d, y_7d, max, na.rm = TRUE) 
      list_m7_day[[i]] <- tapp(pr_7d, ym_7d, which.max)
      list_y7_doy[[i]] <- tapp(pr_7d, y_7d, which.max)
      rm(pr_7d_full, pr_7d)
    }
    
    rm(sub_stack)
    gc() 
  }
  
  # FINAL STITCHING (Only Max Value and Max Date Layers)
  if (window_type %in% c("1d", "all")) {
    writeRaster(rast(list_m1), paste0("monthly_max_prep_", scen_suffix, ".tif"), overwrite = TRUE)
    writeRaster(rast(list_m1_day), paste0("monthly_max_day_prep_", scen_suffix, ".tif"), overwrite = TRUE)
    writeRaster(rast(list_y1), paste0("yearly_max_prep_", scen_suffix, ".tif"), overwrite = TRUE)
    writeRaster(rast(list_y1_doy), paste0("yearly_max_doy_prep_", scen_suffix, ".tif"), overwrite = TRUE)
  }
  if (window_type %in% c("3d", "all")) {
    writeRaster(rast(list_m3), paste0("monthly_max_3day_prep_", scen_suffix, ".tif"), overwrite = TRUE)
    writeRaster(rast(list_m3_day), paste0("monthly_max_day_3day_prep_", scen_suffix, ".tif"), overwrite = TRUE)
    writeRaster(rast(list_y3), paste0("yearly_max_3day_prep_", scen_suffix, ".tif"), overwrite = TRUE) 
    writeRaster(rast(list_y3_doy), paste0("yearly_max_doy_3day_prep_", scen_suffix, ".tif"), overwrite = TRUE) 
  }
  if (window_type %in% c("7d", "all")) {
    writeRaster(rast(list_m7), paste0("monthly_max_7day_prep_", scen_suffix, ".tif"), overwrite = TRUE)
    writeRaster(rast(list_m7_day), paste0("monthly_max_day_7day_prep_", scen_suffix, ".tif"), overwrite = TRUE)
    writeRaster(rast(list_y7), paste0("yearly_max_7day_prep_", scen_suffix, ".tif"), overwrite = TRUE)
    writeRaster(rast(list_y7_doy), paste0("yearly_max_doy_7day_prep_", scen_suffix, ".tif"), overwrite = TRUE)
  }
  print(paste("SUCCESS! Processing complete for:", scen_suffix))
}

# EXECUTION
if (target_dataset == "history") {
  process_optimized(file_history, "1945_2012", window_type = target_window)
} else if (target_dataset == "rcp45") {
  process_optimized(files_rcp45, "RCP45_1945_2099", window_type = target_window)
} else if (target_dataset == "rcp85") {
  process_optimized(files_rcp85, "RCP85_1945_2099", window_type = target_window)
}


#check outputs with plot
pr_3d <- rast("monthly_max_3day_prep_1945_2012.tif")
plot(pr_3d[[1:12]]) # Plot the first 12 layers (




library(terra)

# 1. Define the files you already generated
files_to_filter <- c(
  "yearly_max_doy_3day_prep_1945_2012.tif",
  "yearly_max_3day_prep_1945_2012.tif",
  "monthly_max_day_3day_prep_1945_2012.tif",
  "monthly_max_3day_prep_1945_2012.tif"
)

# (Assuming stations_pts is still in your environment from earlier)
# If not, run this again: 

ds_df <- read_excel("station_coordinate_apr15.xlsx")
stations_pts <- vect(coords_df, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
#visulize the station points to check
plot(stations_pts)
# 2. Loop through each file, apply the 5x5 filter, and save it
for (file in files_to_filter) {
  print(paste("Reading and filtering:", file))
  
  # Read the full-map raster
  r_full <- rast(file)
  
  # --- Recalculate 5x5 grid specifically for the prep raster's geometry ---
  template_layer <- r_full[[1]]
  center_cells <- cells(template_layer, stations_pts)[, "cell"]
  
  # Get 3x3
  ring_1 <- adjacent(template_layer, center_cells, directions = 8, include = TRUE)
  ring_1_cells <- unique(as.vector(ring_1))
  
  # Get 5x5 by finding neighbors of the 3x3
  ring_2 <- adjacent(template_layer, ring_1_cells, directions = 8, include = TRUE)
  keep_cells <- unique(as.vector(ring_2))
  # -----------------------------------------------------------------------
  
  # Apply the filter (set all to NA, then fill in just the 5x5 grid)
  r_filtered <- r_full
  values(r_filtered) <- NA
  r_filtered[keep_cells] <- r_full[keep_cells]
  
  # Create a new file name so we don't overwrite the original full maps just yet
  new_filename <- paste0("filtered_station_", file)
  
  # Save the freshly filtered raster
  writeRaster(r_filtered, new_filename, overwrite = TRUE)
  
  print(paste("Successfully saved:", new_filename))
  
  # Clean up memory
  rm(r_full, r_filtered)
  gc()
}

print("All prep files have been filtered to the 5x5 grid!")

#plot the filtered on map raster to check
r_filtered_check <- rast("filtered_station_yearly_max_3day_prep_1945_2012.tif")
plot(r_filtered_check[[1:12]]) # Plot the first 12 layers to visually confirm the filtering worked
