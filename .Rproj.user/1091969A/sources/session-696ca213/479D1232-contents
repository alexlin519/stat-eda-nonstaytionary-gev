library(terra)
library(readr)
library(dplyr)
library(tidyr)
library(arrow)

# 1. Setup and Memory Management
terraOptions(memmax = 0.4, progress = 10) 

# --- SET YOUR TARGET DATASET AND VARIABLE HERE ---
target_dataset <- "rcp45"  # Change to "rcp45" or "rcp85" for next runs
variable_name <- "baseflow"

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

# Dynamically select input files and set output filenames based on target
if (target_dataset == "history") {
  target_files <- file_history
} else if (target_dataset == "rcp45") {
  target_files <- files_rcp45
} else if (target_dataset == "rcp85") {
  target_files <- files_rcp85
} else {
  stop("Invalid target_dataset! Please use 'history', 'rcp45', or 'rcp85'")
}

spatial_filename <- paste0("daily_bf_", target_dataset, ".tif")
tabular_filename <- paste0("avg_daily_bf_", target_dataset, ".parquet")

# -------------------------------------------------

# Load coordinates
coords_df <- read_csv("stream_coor_station_hist.csv")
unique_stations <- coords_df %>% 
  dplyr::select(Station, Longitude, Latitude) %>% 
  distinct()

# Load the raster based on the selected target
print(paste("Loading raster data for:", target_dataset))
bf_stack <- rast(target_files)[toupper(variable_name)]
template_layer <- bf_stack[[1]]

# 2. Map 5x5 grid boundaries to stations
print("Mapping 5x5 grid boundaries to stations...")
station_cells_list <- lapply(1:nrow(unique_stations), function(i) {
  pt <- vect(unique_stations[i, ], geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
  center_cell <- cells(template_layer, pt)[, "cell"]
  
  ring_1 <- adjacent(template_layer, center_cell, directions = 8, include = TRUE)
  ring_2 <- adjacent(template_layer, as.vector(ring_1), directions = 8, include = TRUE)
  
  return(data.frame(Station = unique_stations$Station[i], Cell_ID = unique(as.vector(ring_2))))
})

station_cell_map <- do.call(rbind, station_cells_list)
keep_cells <- unique(station_cell_map$Cell_ID)

# =========================================================================
# OUTPUT 1: SPATIAL GRIDDED DATA (GeoTIFF)
# =========================================================================
print(paste("Generating Output 1:", spatial_filename))

mask_rast <- template_layer
values(mask_rast) <- NA
mask_rast[keep_cells] <- 1

grid_ext <- ext(mask_rast, cells = keep_cells)
mask_rast_cropped <- crop(mask_rast, grid_ext)

bf_cropped <- crop(bf_stack, grid_ext)
bf_masked <- mask(bf_cropped, mask_rast_cropped)

# Save Output 1 as highly compressed GeoTIFF
writeRaster(bf_masked, spatial_filename, filetype = "GTiff", gdal = "COMPRESS=LZW", overwrite = TRUE)
print("Saved Output 1 successfully!")

# Clean up memory BEFORE starting Output 2
rm(bf_cropped, bf_masked, mask_rast, mask_rast_cropped)
gc()

# =========================================================================
# OUTPUT 2: TABULAR PARQUET (Top 9 Daily Mean)
# =========================================================================
print(paste("Generating Output 2:", tabular_filename))

all_years <- unique(format(time(bf_stack), "%Y"))
# 5-year chunks to save RAM
year_groups <- split(all_years, ceiling(seq_along(all_years) / 5))

chunked_results <- list()

for (i in seq_along(year_groups)) {
  current_years <- year_groups[[i]]
  print(paste0("  Extracting Chunk ", i, "/", length(year_groups), " (Years: ", min(current_years), "-", max(current_years), ")"))
  
  sub_stack <- bf_stack[[format(time(bf_stack), "%Y") %in% current_years]]
  chunk_dates <- time(sub_stack)
  
  extracted_mat <- terra::extract(sub_stack, station_cell_map$Cell_ID)
  
  extracted_df <- as.data.frame(extracted_mat)
  colnames(extracted_df) <- as.character(chunk_dates)
  extracted_df$Station <- station_cell_map$Station
  
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


# =========================================================================
# OUTPUT 3: MERGE BASEFLOW WITH STREAMFLOW
# =========================================================================
library(dplyr)
library(readr)
library(arrow)

# 1. Dynamically set the streamflow file path based on the target_dataset
if (target_dataset == "history") {
  sf_filename <- "/Users/alexlin/Downloads/EDA/sf_yearly_max_hist.csv"
} else {
  # This will seamlessly handle "rcp45" and "rcp85"
  sf_filename <- paste0("/Users/alexlin/Downloads/EDA/sf_yearly_max_", target_dataset, ".csv")
}

# 2. Load the datasets
print(paste("Loading baseflow data from:", tabular_filename))
bf_data <- read_parquet(tabular_filename)

print(paste("Loading streamflow data from:", sf_filename))
sf_max_data <- read_csv(sf_filename)

# 3. Ensure both date columns are standard R Date objects for a safe join
sf_max_data <- sf_max_data %>%
  mutate(MaxDate = as.Date(MaxDate))

bf_data <- bf_data %>%
  mutate(Date = as.Date(Date))

# 4. Merge the Baseflow covariate to the Streamflow Yearly Maxima
# We join by Station, and match the streamflow's 'MaxDate' to the baseflow's 'Date'
gev_input_data <- sf_max_data %>%
  left_join(bf_data, by = c("Station" = "Station", "MaxDate" = "Date")) %>%
  # Rename the column to be clear this is the covariate
  rename(Baseflow_Covariate = Rep_Baseflow)

# 5. Check for any missing joins
missing_covariates <- sum(is.na(gev_input_data$Baseflow_Covariate))
cat("Rows missing a baseflow covariate after merge:", missing_covariates, "\n")

# 6. Save the final merged dataset for modeling
# Dynamically name the final CSV based on your target_dataset variable
merged_output_filename <- paste0("year_", target_dataset, "_sf_bf.csv")
write_csv(gev_input_data, merged_output_filename)

print(paste("Merge complete! Saved as", merged_output_filename, "- Ready for GEV modeling."))

# -------------------------------------------------
# Quick Data Checks
# -------------------------------------------------
head(gev_input_data)
tail(gev_input_data)

# Check missing value for baseflow covariate
print("Total NA in Baseflow_Covariate:")
print(sum(is.na(gev_input_data$Baseflow_Covariate)))

# Check 2013 whole year data 
bf_2013_station_55 <- gev_input_data %>%
  filter(format(MaxDate, "%Y") == "2013") %>% # Using MaxDate since Year might not be a column yet
  filter(Station %in% unique_stations$Station)

print("Preview of 2013 data:")
head(bf_2013_station_55)