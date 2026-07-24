library(terra)
library(readr)
library(dplyr)
library(tidyr)
library(arrow)

# 1. Setup
terraOptions(memmax = 0.4, progress = 10) 

# --- SET YOUR TARGET DATASET HERE ---
target_dataset <- "history"  # Change to "rcp45" or "rcp85" for next runs
variable_name <- "baseflow"

# Define filenames based on target
spatial_filename <- paste0("daily_bf_", target_dataset, ".tif")
tabular_filename <- paste0("avg_daily_bf_", target_dataset, ".parquet")

# Define original NetCDF paths (used ONLY to steal the exact Date metadata)
if (target_dataset == "history") {
  original_files <- "netCDF_data/Gridded 1945-2012 Baseflow.nc"
} else if (target_dataset == "rcp45") {
  original_files <- c("netCDF_data/CanESM2 RCP45 Baseflow 1945-2099 1.nc",
                      "netCDF_data/CanESM2 RCP45 Baseflow 1945-2099 3.nc", 
                      "netCDF_data/CanESM2 RCP45 Baseflow 1945-2099 2.nc")
} else if (target_dataset == "rcp85") {
  original_files <- c("netCDF_data/CanESM2 RCP85 Baseflow 1945-2099 1.nc",
                      "netCDF_data/CanESM2 RCP85 Baseflow 1945-2099 3.nc",
                      "netCDF_data/CanESM2 RCP85 Baseflow 1945-2099 2.nc")
}

# Load coordinates
coords_df <- read_csv("stream_coor_station_hist.csv")
unique_stations <- coords_df %>% 
  select(Station, Longitude, Latitude) %>% 
  distinct()

# -------------------------------------------------

# 2. Load Output 1 and Restore Dates
print(paste("Loading Output 1:", spatial_filename))
bf_cropped_stack <- rast(spatial_filename)

print("Restoring Date metadata from original NetCDF...")
original_stack <- rast(original_files)[toupper(variable_name)]
# Copy the exact time dimension from the original to the new TIF
time(bf_cropped_stack) <- time(original_stack)
rm(original_stack) # Free memory immediately
gc()

# 3. Recalculate the 5x5 mapping for the NEW cropped cell IDs
print("Recalculating 5x5 grid boundaries on the cropped map...")
template_layer <- bf_cropped_stack[[1]]

station_cells_list <- lapply(1:nrow(unique_stations), function(i) {
  pt <- vect(unique_stations[i, ], geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
  
  # Find the cell ID on the new, smaller TIF map
  center_cell <- cells(template_layer, pt)[, "cell"]
  
  # Calculate 5x5 based on the new grid
  ring_1 <- adjacent(template_layer, center_cell, directions = 8, include = TRUE)
  ring_2 <- adjacent(template_layer, as.vector(ring_1), directions = 8, include = TRUE)
  
  return(data.frame(Station = unique_stations$Station[i], Cell_ID = unique(as.vector(ring_2))))
})

station_cell_map <- do.call(rbind, station_cells_list)

# =========================================================================
# OUTPUT 2: TABULAR PARQUET (Top 9 Daily Mean)
# =========================================================================
print(paste("Generating Output 2:", tabular_filename))

all_years <- unique(format(time(bf_cropped_stack), "%Y"))
# We can use slightly larger chunks (e.g., 10 years) now because the spatial file is tiny
year_groups <- split(all_years, ceiling(seq_along(all_years) / 10))

chunked_results <- list()

for (i in seq_along(year_groups)) {
  current_years <- year_groups[[i]]
  print(paste0("  Extracting Chunk ", i, "/", length(year_groups), " (Years: ", min(current_years), "-", max(current_years), ")"))
  
  # Subset time chunk
  sub_stack <- bf_cropped_stack[[format(time(bf_cropped_stack), "%Y") %in% current_years]]
  chunk_dates <- time(sub_stack)
  
  # Extract values using the new Cell IDs
  extracted_mat <- terra::extract(sub_stack, station_cell_map$Cell_ID)
  
  extracted_df <- as.data.frame(extracted_mat)
  colnames(extracted_df) <- as.character(chunk_dates)
  extracted_df$Station <- station_cell_map$Station
  
  # Process Top 9 Mean
  daily_summary <- extracted_df %>%
    pivot_longer(
      cols = -Station, 
      names_to = "Date", 
      values_to = "Baseflow"
    ) %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Station, Date) %>%
    slice_max(order_by = Baseflow, n = 9, with_ties = FALSE) %>%
    summarize(Rep_Baseflow = mean(Baseflow, na.rm = TRUE), .groups = "drop")
  
  chunked_results[[i]] <- daily_summary
  
  # Clean up iteration memory
  rm(sub_stack, extracted_mat, extracted_df, daily_summary)
  gc()
}

# Save Output 2 as Parquet
final_tabular_data <- bind_rows(chunked_results)
write_parquet(final_tabular_data, tabular_filename)

print("Saved Output 2 successfully!")
print("Run complete!")

#get col name for sf yearly max /Users/alexlin/Downloads/EDA/sf_yearly_max_hist.csv

read_csv("/Users/alexlin/Downloads/EDA/sf_yearly_max_hist.csv") %>% colnames()

head(read_csv("/Users/alexlin/Downloads/EDA/sf_yearly_max_hist.csv"))
