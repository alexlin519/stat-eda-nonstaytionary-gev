library(terra)

# ==========================================
# 1. TRANSFER TIF INTO DF, THEN SAVE TO CSV
# ==========================================

# Load your original filtered TIFs
val_original <- rast("filtered_yearly_max_val_History_1945_2012.tif")
doy_original <- rast("filtered_yearly_max_doy_History_1945_2012.tif")

# Convert both to dataframes
# xy = TRUE keeps the Longitude and Latitude centers
# na.rm = TRUE drops all the empty space to keep the CSV file small
val_df <- as.data.frame(val_original, xy = TRUE, na.rm = TRUE)
doy_df <- as.data.frame(doy_original, xy = TRUE, na.rm = TRUE)

# Save them to CSV files
write.csv(val_df, "test_val_export.csv", row.names = FALSE)
write.csv(doy_df, "test_doy_export.csv", row.names = FALSE)

print("Step 1 Complete: CSVs saved.")

# ==========================================
# 2. TRANSFER CSV BACK INTO TIF
# ==========================================

# Read the CSVs back into R (simulating starting from scratch)
imported_val_df <- read.csv("test_val_export.csv")
imported_doy_df <- read.csv("test_doy_export.csv")

# Convert the dataframes back into spatial rasters
# type = "xyz" tells terra that col 1 is X, col 2 is Y, and the rest are layers
restored_val_rast <- rast(imported_val_df, type = "xyz", crs = "EPSG:4326")
restored_doy_rast <- rast(imported_doy_df, type = "xyz", crs = "EPSG:4326")

# Save the restored rasters as NEW .tif files to prove they work
writeRaster(restored_val_rast, "RESTORED_yearly_max_val_History.tif", overwrite = TRUE)
writeRaster(restored_doy_rast, "RESTORED_yearly_max_doy_History.tif", overwrite = TRUE)

print("Step 2 Complete: New TIFs generated from CSVs.")