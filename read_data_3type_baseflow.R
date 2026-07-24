library(terra)

# 1. Setup and Memory Management
terraOptions(memmax = 0.4, progress = 10) 

# --- SET YOUR TARGET DATASET HERE ---
# Change this variable to "history", "rcp45", or "rcp85"
target_dataset <- "history" 
# ------------------------------------

# Define file paths with your updated filenames
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

# 2. Create the reusable processing function
process_climate_scenario <- function(file_vector, scenario_name) {
  print(paste("Starting processing for:", scenario_name))
  
  # Load files as one continuous stack
  bf_stack <- rast(file_vector)["BASEFLOW"]
  
  # Get unique years across the entire period
  all_years <- unique(format(time(bf_stack), "%Y"))
  year_groups <- split(all_years, ceiling(seq_along(all_years) / 10))
  
  # Result storage
  monthly_max_list <- list()
  yearly_max_list <- list()
  
  for (i in seq_along(year_groups)) {
    current_years <- year_groups[[i]]
    print(paste0("Processing Group ", i, "/", length(year_groups), 
                 " (Years: ", min(current_years), "-", max(current_years), ")"))
    
    # Subset Raw Data
    sub_stack <- bf_stack[[format(time(bf_stack), "%Y") %in% current_years]]
    
    # STEP A: Calculate Monthly Max
    idx_monthly <- format(time(sub_stack), "%Y-%m")
    chunk_monthly <- tapp(sub_stack, index = idx_monthly, fun = max, na.rm = TRUE)
    monthly_max_list[[i]] <- chunk_monthly
    
    # STEP B: Calculate Yearly Max
    unique_months <- unique(idx_monthly)
    idx_yearly_from_month <- substr(unique_months, 1, 4)
    chunk_yearly <- tapp(chunk_monthly, index = idx_yearly_from_month, fun = max, na.rm = TRUE)
    yearly_max_list[[i]] <- chunk_yearly
    
    # Clean up
    rm(sub_stack, chunk_monthly, chunk_yearly)
    gc() 
  }
  
  print(paste("Loop finished for", scenario_name, "Combining and Saving..."))
  
  # --- Process Monthly Max Final ---
  final_monthly_max <- rast(monthly_max_list)
  monthly_filename <- paste0("monthly_max_", scenario_name, ".tif")
  writeRaster(final_monthly_max, monthly_filename, overwrite = TRUE)
  print(paste("Saved:", monthly_filename))
  
  # --- Process Yearly Max Final ---
  final_yearly_max <- rast(yearly_max_list)
  
  if (nlyr(final_yearly_max) == length(all_years)) {
    names(final_yearly_max) <- all_years
  } else {
    warning(paste("Layer count mismatch on yearly stack for", scenario_name))
  }
  
  yearly_filename <- paste0("yearly_max_", scenario_name, ".tif")
  writeRaster(final_yearly_max, yearly_filename, overwrite = TRUE)
  print(paste("Saved:", yearly_filename))
  
  return(final_yearly_max)
}

# 3. Execute based on the variable assignment
if (target_dataset == "history") {
  result <- process_climate_scenario(file_history, "History_1945_2012")
} else if (target_dataset == "rcp45") {
  result <- process_climate_scenario(files_rcp45, "RCP45_1945_2099")
} else if (target_dataset == "rcp85") {
  result <- process_climate_scenario(files_rcp85, "RCP85_1945_2099")
} else {
  print("Invalid target_dataset. Please set it to 'history', 'rcp45', or 'rcp85'.")
}










# read the tif files back in to check
monthly_max_history <- rast("monthly_max_History_1945_2012.tif")
yearly_max_history <- rast("yearly_max_History_1945_2012.tif")
monthly_max_rcp45 <- rast("monthly_max_RCP45_1945_2099.tif")
yearly_max_rcp45 <- rast("yearly_max_RCP45_1945_2099.tif")
monthly_max_rcp85 <- rast("monthly_max_RCP85_1945_2099.tif")
yearly_max_rcp85 <- rast("yearly_max_RCP85_1945_2099.tif")


# 1. Get the number of layers
num_layers <- nlyr(yearly_max_rcp45)

# 2. Create a sequence of years 
start_year <- 1945 
years_seq <- start_year : (start_year + num_layers - 1)

# 3. Assign the time attribute
time(yearly_max_rcp45) <- as.Date(paste0(years_seq, "-01-01"))
time(yearly_max_rcp85) <- as.Date(paste0(years_seq, "-01-01"))

# 4. Now run your check again
if (nlyr(yearly_max_rcp45) == length(unique(format(time(yearly_max_rcp45), "%Y")))) {
  message("Success: Layer count matches unique years.")
}

# # Verify layer count matches year count
if (nlyr(yearly_max_history) == length(unique(format(time(yearly_max_history), "%Y")))) {
  print("Yearly max history layer count matches unique years.")
} else {
  warning("Yearly max history layer count does NOT match unique years.")
}


if (nlyr(yearly_max_rcp45) == length(unique(format(time(yearly_max_rcp45), "%Y")))) {
  print("Yearly max RCP45 layer count matches unique years.")
} else {
  warning("Yearly max RCP45 layer count does NOT match unique years.")
}


if (nlyr(yearly_max_rcp85) == length(unique(format(time(yearly_max_rcp85), "%Y")))) {
  print("Yearly max RCP85 layer count matches unique years.")
} else {
  warning("Yearly max RCP85 layer count does NOT match unique years.")
}



# Quick visual check of the first layer of each stack to confirm they look reasonable
par(mfrow=c(1,2))
plot(monthly_max_rcp45[[1]], main = "First Month Max")
plot(yearly_max_rcp45[[1]], main = "First Year Max")

plot(monthly_max_rcp85[[1]], main = "First Month Max")
plot(yearly_max_rcp85[[1]], main = "First Year Max")


#check the saved file, make sure there are 68 layers (1945-2012)
print(monthly_max_rcp45)
print(res(monthly_max_rcp45))
print(res(yearly_max_rcp45))
print(res(monthly_max_rcp85))
print(res(yearly_max_rcp85))

print(tail(names(yearly_max_rcp45), 10))
print(tail(names(yearly_max_rcp85), 10))



# Check units of all layers
#units(bf_stack)

# Check if there's a specific unit for the first layer
#units(bf_stack[[1]])


#check the head
