

library(dplyr)

# 1. Define the path to your folder
folder_path <- "model_flow"

# 2. Get a list of all the file paths in that folder
file_list <- list.files(path = folder_path, pattern = "\\.ascii$", full.names = TRUE)

# 3. Create a custom function to process a single file
process_model_data <- function(file_path) {
  
  # Read the CSV, skipping the first line as requested
  df <- read.csv(file_path, skip = 1)
  
  # Extract the station name from the file path
  # basename() grabs just the filename, and sub() removes everything after the first "."
  # Example: "ORAOR.csv.ascii" becomes "ORAOR"
  station_name <- sub("\\..*", "", basename(file_path))
  
  # Select the required columns and add the new 'station' column
  df_filtered <- df %>%
    select(Date, CanESM2_rcp45_r1i1p1, CanESM2_rcp85_r1i1p1) %>%
    mutate(station = station_name)
  
  return(df_filtered)
}

# 4. Apply the function to all files and combine them into one large dataframe
combined_df <- bind_rows(lapply(file_list, process_model_data))

# Check the final result
head(combined_df)


library(dplyr)

# 1. Create the mapping dataframe manually from the images
station_mapping <- data.frame(
  station = c("FRSMT", "FRSPM", "BCHAL", "BCHSF", "FRSMS", "CHILLI", "HARRI",
              "KWRNW", "FRAAR", "ARNT7", "IRASR", "DRIFT", "ORAAC", "MRAGC", "ORNEL", "ORAOR",
              "PRAPR", "HERNN", "SMRAW", "LSRNG", "WRNTM", "WRNGP", "RRNRG",
              "NICOL", "SPIUS", "MISSI", "THOMK", "STHOM", "BCHWL",
              "ELKFE", "BULWA", "KOOTE", "DCD", "LARMA", "SLOCR", "BRI", "MUC"),
  lon = c(-123.08685, -122.77872, -122.46481, -122.34631, -122.28382, -121.97042, -121.84556,
          -125.63054, -125.27905, -124.91269, -125.20720, -126.64683, -123.96396, -124.63367, -124.76582, -124.58542,
          -117.27826, -117.02779, -117.58592, -117.14382, -117.21800, -118.76401, -119.71959,
          -121.21773, -121.02791, -119.34082, -120.33583, -119.71492, -118.77605,
          -115.57021, -115.33673, -115.65341, -116.96899, -117.02914, -117.59346, -117.53381, -117.65719),
  lat = c(49.10009, 49.22767, 49.29773, 49.24144, 49.16099, 49.10814, 49.29475,
          57.51311, 57.19361, 57.25140, 56.74915, 56.00467, 56.58203, 56.25653, 56.14279, 55.94911,
          56.23391, 56.04523, 55.79941, 55.48742, 54.73082, 55.11266, 55.11255,
          50.35504, 50.10287, 49.85053, 50.66886, 50.79130, 50.28844,
          49.25466, 49.47518, 49.60197, 50.41096, 50.34871, 49.47206, 49.41061, 49.28666)
)

# 2. Join the mapping to your existing combined_df
combined_df_mapped <- combined_df %>%
  left_join(station_mapping, by = "station") %>%
  # Adding the combined location column as requested
  mutate(location = paste(lon, lat, sep = ", "))


#change the order of the columns, location station then lon and lat
combined_df_mapped <- combined_df_mapped %>%
  select(Date, CanESM2_rcp45_r1i1p1, CanESM2_rcp85_r1i1p1, location, station, lon, lat)
# Check the output
head(combined_df_mapped)

#size of combined_df_mapped
print(paste("Number of rows in combined_df_mapped:", nrow(combined_df_mapped)))

