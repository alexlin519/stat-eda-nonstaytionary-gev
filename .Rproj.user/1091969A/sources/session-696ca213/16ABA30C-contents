library(terra)
library(readr)
library(dplyr)
library(tidyr)
library(arrow)
# install.packages("slider") if you don't have it
library(slider) 

# 1. Setup and Memory Management
terraOptions(memmax = 0.4, progress = 10) 

# --- SET YOUR TARGET DATASET AND VARIABLE HERE ---
target_dataset <- "rcp45"  # Change to "rcp45" or "rcp85" for next runs
variable_name <- "pr"        # Change this to whatever the precipitation variable is called in your NetCDF (e.g., "pr", "precip")

# --- ROLLING WINDOW SETUP ---
window_length <- 3
window_align <- "right" # Options: 
# "right"  = Past/Ending on current day (e.g., Day 1, 2, 3 for Day 3)
# "center" = Centered on current day (e.g., Day 2, 3, 4 for Day 3)
# "left"   = Future/Starting on current day (e.g., Day 3, 4, 5 for Day 3)

# Calculate padding needed for chunk boundaries based on alignment
if (window_align == "right") {
  pad_before <- window_length - 1
  pad_after <- 0
} else if (window_align == "center") {
  pad_before <- floor((window_length - 1) / 2)
  pad_after <- ceiling((window_length - 1) / 2)
} else if (window_align == "left") {
  pad_before <- 0
  pad_after <- window_length - 1
}

# Define file paths
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

# Dynamically select input files and set output filenames
if (target_dataset == "history") {
  target_files <- file_history
} else if (target_dataset == "rcp45") {
  target_files <- files_rcp45
} else if (target_dataset == "rcp85") {
  target_files <- files_rcp85
} else {
  stop("Invalid target_dataset! Please use 'history', 'rcp45', or 'rcp85'")
}

spatial_filename <- paste0("daily_raw_", variable_name, "_", target_dataset, ".tif")
tabular_filename <- paste0("avg_daily_roll_", variable_name, "_", target_dataset, ".parquet")

# -------------------------------------------------

# Load coordinates
coords_df <- read_csv("stream_coor_station_hist.csv")
unique_stations <- coords_df %>% 
  dplyr::select(Station, Longitude, Latitude) %>% 
  distinct()

# Load the raster based on the selected target
print(paste("Loading raster data for:", target_dataset))
prep_stack <- rast(target_files)[toupper(variable_name)]
prep_stack <- clamp(prep_stack, lower = 0)
template_layer <- prep_stack[[1]]

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
print(paste("Generating Output 1 (Raw Spatial File):", spatial_filename))

mask_rast <- template_layer
values(mask_rast) <- NA
mask_rast[keep_cells] <- 1

grid_ext <- ext(mask_rast, cells = keep_cells)
mask_rast_cropped <- crop(mask_rast, grid_ext)

prep_cropped <- crop(prep_stack, grid_ext)
prep_masked <- mask(prep_cropped, mask_rast_cropped)

# Save Output 1 as highly compressed GeoTIFF (This saves the RAW prep, not rolling)
writeRaster(prep_masked, spatial_filename, filetype = "GTiff", gdal = "COMPRESS=LZW", overwrite = TRUE)
print("Saved Output 1 successfully!")

# Clean up memory BEFORE starting Output 2
rm(prep_cropped, prep_masked, mask_rast, mask_rast_cropped)
gc()

# =========================================================================
# OUTPUT 2: TABULAR PARQUET (Rolling Avg -> Top 9 Mean)
# =========================================================================
print(paste("Generating Output 2 (Smoothed Tabular Data):", tabular_filename))

all_time <- time(prep_stack)
all_years <- unique(format(all_time, "%Y"))
year_groups <- split(all_years, ceiling(seq_along(all_years) / 5))

chunked_results <- list()

for (i in seq_along(year_groups)) {
  current_years <- year_groups[[i]]
  print(paste0("  Extracting Chunk ", i, "/", length(year_groups), " (Years: ", min(current_years), "-", max(current_years), ")"))
  
  # Find indices of the exact years in this chunk
  target_idx <- which(format(all_time, "%Y") %in% current_years)
  
  # Add padding to indices so rolling average works at chunk boundaries
  start_idx <- max(1, target_idx[1] - pad_before)
  end_idx <- min(nlyr(prep_stack), tail(target_idx, 1) + pad_after)
  
  sub_stack <- prep_stack[[start_idx:end_idx]]
  chunk_dates <- time(sub_stack)
  
  # Extract
  extracted_mat <- terra::extract(sub_stack, station_cell_map$Cell_ID)
  
  extracted_df <- as.data.frame(extracted_mat)
  colnames(extracted_df) <- as.character(chunk_dates)
  # IMPORTANT: Keep both Station and Cell_ID so we can group correctly for time-series smoothing
  extracted_df$Station <- station_cell_map$Station
  extracted_df$Cell_ID <- station_cell_map$Cell_ID
  
  daily_summary <- extracted_df %>%
    pivot_longer(
      cols = -c(Station, Cell_ID), 
      names_to = "Date", 
      values_to = "Raw_Prep"
    ) %>%
    mutate(Date = as.Date(Date)) %>%
    arrange(Station, Cell_ID, Date) %>%
    # 1. Apply the rolling average for each individual grid cell
    group_by(Station, Cell_ID) %>%
    mutate(
      Roll_Prep = slide_dbl(
        Raw_Prep, 
        mean,            # Using 'mean'. If you want rolling sum, change this to 'sum'
        na.rm = TRUE, 
        .before = pad_before, 
        .after = pad_after, 
        .complete = FALSE # Allows partial calculations at the very beginning/end of the whole dataset
      )
    ) %>%
    ungroup() %>%
    # 2. Discard the boundary padding so we don't duplicate days across chunks
    filter(format(Date, "%Y") %in% current_years) %>%
    # 3. Apply your spatial filter: Find top 9 smoothed cells per day and average them
    group_by(Station, Date) %>%
    slice_max(order_by = Roll_Prep, n = 9, with_ties = FALSE) %>%
    summarize(Rep_Prep = mean(Roll_Prep, na.rm = TRUE), .groups = "drop")
  
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


#check the data
final_tabular_data <- read_parquet(tabular_filename)
#check randome ly


sample_rows <- final_tabular_data %>% sample_n(10)
print(sample_rows)

#check all Rep_Prep dist (regradless of the date)
hist(final_tabular_data$Rep_Prep, breaks = 50, main = "Distribution of Representative Precipitation", xlab = "Rep_Prep", ylab = "Frequency")

#in table form
summary(final_tabular_data$Rep_Prep)






# Dynamically construct the file paths based on the target dataset
parquet_path <- paste0("avg_daily_roll_", variable_name, "_", target_dataset, ".parquet")
tif_path <- paste0("daily_raw_", variable_name, "_", target_dataset, ".tif")

print(paste("Applying zero-clamp fix for dataset:", target_dataset))


# --- 1. Fix the Parquet File (Tabular) ---
print(paste("Fixing Parquet:", parquet_path))
if (file.exists(parquet_path)) {
  df <- read_parquet(parquet_path) %>%
    mutate(Rep_Prep = pmax(Rep_Prep, 0))
  write_parquet(df, parquet_path)
  print("  -> Parquet file fixed!")
} else {
  print("  -> Parquet file not found. Skipping.")
}

# --- 2. Fix the TIF File (Raster) ---
print(paste("Fixing TIF:", tif_path))
if (file.exists(tif_path)) {
  # Load the raster
  r <- rast(tif_path)
  
  # Clamp all values below 0 to exactly 0
  r_fixed <- clamp(r, lower = 0)
  
  # Overwrite the TIF file
  writeRaster(r_fixed, tif_path, overwrite = TRUE)
  print("  -> TIF file fixed!")
} else {
  print("  -> TIF file not found. Skipping.")
}

print("Fix pipeline complete!")




# =========================================================================
# OUTPUT 3: MERGE PRECIPITATION WITH STREAMFLOW
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
# Dynamically load the precipitation file you JUST created in Output 2
print(paste("Loading precipitation data from:", tabular_filename))
pr_data <- read_parquet(tabular_filename)

print(paste("Loading streamflow data from:", sf_filename))
sf_max_data <- read_csv(sf_filename)

# 3. Ensure both date columns are standard R Date objects for a safe join
sf_max_data <- sf_max_data %>%
  mutate(MaxDate = as.Date(MaxDate))

pr_data <- pr_data %>%
  mutate(Date = as.Date(Date))

# 4. Merge the Precipitation covariate to the Streamflow Yearly Maxima
# We join by Station, and match the streamflow's 'MaxDate' to the prep's 'Date'
gev_input_data <- sf_max_data %>%
  left_join(pr_data, by = c("Station" = "Station", "MaxDate" = "Date")) %>%
  # Rename the column to match the precipitation data
  rename(Prep_Covariate = Rep_Prep)

# 5. Check for any missing joins
missing_covariates <- sum(is.na(gev_input_data$Prep_Covariate))
cat("Rows missing a precipitation covariate after merge:", missing_covariates, "\n")

# 6. Save the final merged dataset for modeling
# Dynamically name the final CSV based on your target_dataset variable
merged_output_filename <- paste0("year_", target_dataset, "_sf_pr.csv")
write_csv(gev_input_data, merged_output_filename)

print(paste("Merge complete! Saved as", merged_output_filename, "- Ready for GEV modeling."))

# -------------------------------------------------
# Quick Data Checks
# -------------------------------------------------
head(gev_input_data)
tail(gev_input_data)

# Check missing value for prep covariate
print("Total NA in Prep_Covariate:")
print(sum(is.na(gev_input_data$Prep_Covariate)))