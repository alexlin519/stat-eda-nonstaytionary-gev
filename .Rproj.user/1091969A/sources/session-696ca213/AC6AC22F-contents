# Load required libraries
library(tidyverse)
library(readxl)

# 1. Load the coordinate excel file
# (R might print rounded numbers in the console, but the full decimals are safe!)
coords_df <- read_excel("station_coordinate_apr15.xlsx")
coords_stations <- coords_df$`Location Code`
#coords_stations
colnames(coords_df)
tail(coords_df)
#size of df
cat("Number of stations in Excel coordinates file:", nrow(coords_df), "\n")
# 2. Get a list of all your station data files in the "data" folder
file_list <- list.files(path = "model_flow", pattern = "\\.csv\\.ascii$", full.names = TRUE)
file_stations <- str_extract(basename(file_list), "^[^.]+")

# 3. PRE-MERGE CHECK: Identify missing data or missing coordinates
missing_data <- setdiff(coords_stations, file_stations)
missing_coords <- setdiff(file_stations, coords_stations)

cat("\n--- PRE-MERGE DIAGNOSTICS ---\n")
if (length(missing_data) > 0) {
  cat("WARNING: Stations in Excel but MISSING CSV files in 'data' folder:\n")
  print(missing_data)
} else {
  cat("SUCCESS: All stations in Excel have corresponding CSV files.\n")
}

if (length(missing_coords) > 0) {
  cat("\nWARNING: Stations with CSV files but MISSING from Excel coordinates:\n")
  print(missing_coords)
} else {
  cat("\nSUCCESS: All CSV files have matching coordinates in Excel.\n")
}
cat("-----------------------------\n\n")

# 4. Filter to ONLY keep files that have a match (ignores both missing cases)
valid_stations <- intersect(file_stations, coords_stations)
valid_files <- file_list[file_stations %in% valid_stations]






#debug

# Check the first file to see what columns it actually has
sample_file <- valid_files[1]
cat("Inspecting:", sample_file, "\n\n")

# Print the first several raw lines to see the structure
cat("--- First 10 raw lines ---\n")
readLines(sample_file, n = 10)

# Try reading it and see what columns exist
sample_df <- read_csv(sample_file, show_col_types = FALSE)
cat("\n--- Column names ---\n")
print(colnames(sample_df))
cat("\n--- First few rows ---\n")
print(head(sample_df))







# 5. Create the processing function
process_station <- function(file_path) {
  station_name <- str_extract(basename(file_path), "^[^.]+")
  
  # Skip the "sequence" junk line; line 2 becomes the header
  df <- read_csv(file_path, skip = 1, show_col_types = FALSE)
  
  df_filtered <- df %>%
    select(Date, PNWNAmet, CanESM2_rcp45_r1i1p1, CanESM2_rcp85_r1i1p1) %>%
    mutate(Date = as.Date(Date, format = "%Y/%m/%d"),
           Station = station_name)
  
  return(df_filtered)
}

# 6. Read and combine ONLY the valid files
cat(paste("Processing", length(valid_files), "valid stations...\n"))
all_station_data <- map_dfr(valid_files, process_station)

# 7. Merge coordinates 
# We join 'Station' from the data files to 'Location Code' from the Excel file
final_merged_data <- all_station_data %>%
  inner_join(coords_df, by = c("Station" = "Location Code")) %>%
  relocate(Station, Longitude, Latitude)

colnames(final_merged_data)
# 8. Save your final combined data file
# write_csv will save the full, unrounded decimal numbers!
# 3 time, so big!!!
write_csv(final_merged_data, "stream_coor_station_3_model.csv")
cat("\nMerge complete! Ignored mismatches and saved with full decimal 
    precision to 'FINAL_merged_station_data.csv'\n")

#read the big file back in to split into 3 smaller files for the web app, since the web app only needs one column at a time, no need to load all 3 columns at once
stream_coor_station_3_model <- read_csv("stream_coor_station_3_model.csv")

# get only the PNWNAmet, no need for CanESM2_rcp45_r1i1p1, CanESM2_rcp85_r1i1p1
# stream_coor_station_3_model is the full final data name
stream_station_hist <- stream_coor_station_3_model %>%
  select(Station, Longitude, Latitude, Date, PNWNAmet)
write_csv(stream_station_hist, "stream_coor_station_hist.csv")

# get only the CanESM2_rcp45_r1i1p1
stream_station_rcp45 <- stream_coor_station_3_model %>%
  select(Station, Longitude, Latitude, Date, CanESM2_rcp45_r1i1p1)
write_csv(stream_station_rcp45, "stream_coor_station_rcp45.csv")

# get only the CanESM2_rcp85_r1i1p1
stream_station_rcp85 <- stream_coor_station_3_model %>%
  select(Station, Longitude, Latitude, Date, CanESM2_rcp85_r1i1p1)
write_csv(stream_station_rcp85, "stream_coor_station_rcp85.csv")

#delete the big file to save space
#file.remove("stream_coor_station_3_model.csv")

# use stream_coor_station_3_model.csv, to check if the history date the 3 model value 
# are the same, if not, then there is a problem with the merge
# check the first few rows of the history column and the 3 model columns to see if they match
check_df <- stream_coor_station_3_model %>%
  select(Date, PNWNAmet, CanESM2_rcp45_r1i1p1, CanESM2_rcp85_r1i1p1) %>%
  head()
print(check_df)

# use a blooean summary to check if the history column matches the 3 model columns for the same date, if not, then there is a problem with the merge
# i want to see how many diff of date in totaly the hist and rcp45 is diff in terms of the value
date_mismatch_hist_rcp45 <- stream_coor_station_3_model %>%
  filter(PNWNAmet != CanESM2_rcp45_r1i1p1) %>%
  summarise(count = n())
cat("\nNumber of date mismatches between history and rcp45:", date_mismatch_hist_rcp45$count, "\n")    
# give me the proportion of date mismatches between history and rcp45
total_rows <- nrow(stream_coor_station_3_model)
proportion_mismatch_hist_rcp45 <- date_mismatch_hist_rcp45$count / total_rows
cat("Proportion of date mismatches between history and rcp45:", proportion_mismatch_hist_rcp45, "\n")

# 