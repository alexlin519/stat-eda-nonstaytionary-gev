
library(terra)
library(readxl)
library(readr)
# 1. Setup and Memory Management
terraOptions(memmax = 0.4, progress = 10) 

# --- SET YOUR TARGET DATASET AND VARIABLE HERE ---
target_dataset <- "rcp45"  # Change to "history" or "rcp85" for other runs 
variable_name <- "baseflow"
# Load the station coordinates and streamflow from the Excel file
coords_df <- read_csv("stream_coor_station_hist.csv")

# -------------------------------------------------

# Define file paths
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


# Prep the streamflow dataframe for the lookup step
coords_df$Date <- as.Date(coords_df$Date)
coords_df$YM <- format(coords_df$Date, "%Y-%m")
coords_df$Y <- format(coords_df$Date, "%Y")
coords_df$Day <- as.numeric(format(coords_df$Date, "%d"))
coords_df$DOY <- as.numeric(format(coords_df$Date, "%j"))

# Convert to spatial points (remembering to use DD format for coordinates)
stations_pts <- vect(coords_df[1, ], geom = c("Longitude", "Latitude"), crs = "EPSG:4326")

# 2. Create the reusable processing function
process_climate_scenario <- function(file_vector, scenario_name, stations, streamflow_data, target_var) {
  print(paste("Starting processing for:", scenario_name))
  
  # Load files as one continuous stack
  bf_stack <- rast(file_vector)[toupper(target_var)]
  
  # --- Calculate 5x5 grid boundaries for stations ---
  print("Calculating 5x5 grid boundaries for stations...")
  template_layer <- bf_stack[[1]]
  center_cells <- cells(template_layer, stations)[, "cell"]
  
  # Get 3x3
  ring_1 <- adjacent(template_layer, center_cells, directions = 8, include = TRUE)
  ring_1_cells <- unique(as.vector(ring_1))
  
  # Get 5x5 by finding neighbors of the 3x3
  ring_2 <- adjacent(template_layer, ring_1_cells, directions = 8, include = TRUE)
  keep_cells <- unique(as.vector(ring_2))
  # --------------------------------------------------
  
  all_years <- unique(format(time(bf_stack), "%Y"))
  
  year_groups <- split(all_years, ceiling(seq_along(all_years) / 10))
  
  monthly_var_list <- list()
  monthly_date_list <- list()
  monthly_stream_list <- list()
  
  yearly_var_list <- list()
  yearly_date_list <- list()
  yearly_stream_list <- list()
  
  for (i in seq_along(year_groups)) {
    current_years <- year_groups[[i]]
    print(paste0("Processing Group ", i, "/", length(year_groups)))
    
    sub_stack <- bf_stack[[format(time(bf_stack), "%Y") %in% current_years]]
    
    # Define time indices
    idx_monthly <- format(time(sub_stack), "%Y-%m")
    idx_yearly  <- format(time(sub_stack), "%Y")
    
    # --- Calculate Monthly Max and Day of Month ---
    chunk_monthly_val <- tapp(sub_stack, index = idx_monthly, fun = max, na.rm = TRUE)
    chunk_monthly_day <- tapp(sub_stack, index = idx_monthly, fun = which.max)
    
    # Filter Monthly Values
    chunk_monthly_filtered <- chunk_monthly_val
    values(chunk_monthly_filtered) <- NA
    chunk_monthly_filtered[keep_cells] <- chunk_monthly_val[keep_cells]
    monthly_var_list[[i]] <- chunk_monthly_filtered
    
    # Filter Monthly Dates
    chunk_monthly_day_filtered <- chunk_monthly_day
    values(chunk_monthly_day_filtered) <- NA
    chunk_monthly_day_filtered[keep_cells] <- chunk_monthly_day[keep_cells]
    monthly_date_list[[i]] <- chunk_monthly_day_filtered
    
    # LOOKUP: Create Monthly Streamflow Raster
    chunk_stream_m_list <- list()
    for (lyr_idx in 1:nlyr(chunk_monthly_day_filtered)) {
      lyr_name <- names(chunk_monthly_day_filtered)[lyr_idx] 
      lyr_data <- chunk_monthly_day_filtered[[lyr_idx]]
      
      sf_subset <- streamflow_data[streamflow_data$YM == lyr_name, c("Day", "PNWNAmet")]
      if (nrow(sf_subset) > 0) {
        chunk_stream_m_list[[lyr_idx]] <- subst(lyr_data, from = sf_subset$Day, to = sf_subset$PNWNAmet)
      } else {
        empty_lyr <- lyr_data
        values(empty_lyr) <- NA
        chunk_stream_m_list[[lyr_idx]] <- empty_lyr
      }
    }
    monthly_stream_list[[i]] <- rast(chunk_stream_m_list)
    
    # --- Calculate Yearly Max and Day of Year ---
    chunk_yearly_val <- tapp(sub_stack, index = idx_yearly, fun = max, na.rm = TRUE)
    chunk_yearly_doy <- tapp(sub_stack, index = idx_yearly, fun = which.max)
    
    # Filter Yearly Values
    chunk_yearly_filtered <- chunk_yearly_val
    values(chunk_yearly_filtered) <- NA
    chunk_yearly_filtered[keep_cells] <- chunk_yearly_val[keep_cells]
    yearly_var_list[[i]] <- chunk_yearly_filtered
    
    # Filter Yearly Dates
    chunk_yearly_doy_filtered <- chunk_yearly_doy
    values(chunk_yearly_doy_filtered) <- NA
    chunk_yearly_doy_filtered[keep_cells] <- chunk_yearly_doy[keep_cells]
    yearly_date_list[[i]] <- chunk_yearly_doy_filtered
    
    # LOOKUP: Create Yearly Streamflow Raster
    chunk_stream_y_list <- list()
    for (lyr_idx in 1:nlyr(chunk_yearly_doy_filtered)) {
      lyr_name <- names(chunk_yearly_doy_filtered)[lyr_idx] 
      lyr_data <- chunk_yearly_doy_filtered[[lyr_idx]]
      
      sf_subset <- streamflow_data[streamflow_data$Y == lyr_name, c("DOY", "PNWNAmet")]
      if (nrow(sf_subset) > 0) {
        chunk_stream_y_list[[lyr_idx]] <- subst(lyr_data, from = sf_subset$DOY, to = sf_subset$PNWNAmet)
      } else {
        empty_lyr <- lyr_data
        values(empty_lyr) <- NA
        chunk_stream_y_list[[lyr_idx]] <- empty_lyr
      }
    }
    yearly_stream_list[[i]] <- rast(chunk_stream_y_list)
    
    rm(sub_stack, chunk_monthly_val, chunk_monthly_day, chunk_yearly_val, chunk_yearly_doy)
    gc() 
  }
  
  # Final Rasters
  final_monthly_var <- rast(monthly_var_list)
  final_monthly_day <- rast(monthly_date_list)
  final_monthly_stream <- rast(monthly_stream_list)
  
  final_yearly_var  <- rast(yearly_var_list)
  final_yearly_doy  <- rast(yearly_date_list)
  final_yearly_stream <- rast(yearly_stream_list)
  
  # Write Files 
  #
  writeRaster(final_monthly_var, paste0( "monthly_",target_var,"_", scenario_name, ".tif"), overwrite = TRUE)
  writeRaster(final_monthly_day, paste0("monthly_max_day_", scenario_name, ".tif"), overwrite = TRUE)
  writeRaster(final_monthly_stream, paste0( "monthly_stream_", scenario_name, ".tif"), overwrite = TRUE)
  
  if (nlyr(final_yearly_var) == length(all_years)) {
    names(final_yearly_var) <- all_years
    names(final_yearly_doy) <- all_years
    names(final_yearly_stream) <- all_years
  }
  
  writeRaster(final_yearly_var, paste0( "yearly_",target_var,"_", scenario_name, ".tif"), overwrite = TRUE)
  writeRaster(final_yearly_doy, paste0( "yearly_max_doy_", scenario_name, ".tif"), overwrite = TRUE)
  writeRaster(final_yearly_stream, paste0( "yearly_stream_", scenario_name, ".tif"), overwrite = TRUE)
  
  return(list(
    monthly_var = final_monthly_var, 
    monthly_day = final_monthly_day,
    monthly_stream = final_monthly_stream,
    yearly_var = final_yearly_var, 
    yearly_doy = final_yearly_doy,
    yearly_stream = final_yearly_stream
  ))
}

# 3. Execute
if (target_dataset == "history") {
  result <- process_climate_scenario(file_history, "History_1945_2012", stations_pts, coords_df, variable_name)
} else if (target_dataset == "rcp45") {
  result <- process_climate_scenario(files_rcp45, "RCP45_1945_2099", stations_pts, coords_df, variable_name)
} else if (target_dataset == "rcp85") {
  result <- process_climate_scenario(files_rcp85, "RCP85_1945_2099", stations_pts, coords_df, variable_name)
}


# how manny station in total in stream_coor_station_hist.csv
total_stations <- nrow(unique(coords_df[, c("Station", "Longitude", "Latitude")]))
cat("Total unique stations in stream_coor_station_hist.csv:", total_stations, "\n")

