
# ============================================================
# Block Maximum Extraction (Monthly / Yearly) for 3 models
# ============================================================
# Run ONE combination at a time by changing the two variables below.
# Output: 1 CSV per run. Run 6 times total to get all combinations.
#
#   period_type = "monthly" or "yearly"
#   model_type  = "hist", "rcp45", or "rcp85"
# ============================================================

library(tidyverse)
library(lubridate)

# ---- 1. CONTROL VARIABLES (change these to pick the run) ----
period_type <- "yearly"   # "monthly" or "yearly"
model_type  <- "rcp85"     # "hist", "rcp45", or "rcp85"

# ---- 2. Map model_type to the correct input file & column ----
model_config <- list(
  hist  = list(file = "stream_coor_station_hist.csv",  col = "PNWNAmet"),
  rcp45 = list(file = "stream_coor_station_rcp45.csv", col = "CanESM2_rcp45_r1i1p1"),
  rcp85 = list(file = "stream_coor_station_rcp85.csv", col = "CanESM2_rcp85_r1i1p1")
)

if (!model_type %in% names(model_config)) {
  stop("model_type must be one of: hist, rcp45, rcp85")
}
if (!period_type %in% c("monthly", "yearly")) {
  stop("period_type must be either 'monthly' or 'yearly'")
}

input_file <- model_config[[model_type]]$file
flow_col   <- model_config[[model_type]]$col

cat("Run config:\n")
cat("  period_type :", period_type, "\n")
cat("  model_type  :", model_type, "\n")
cat("  input file  :", input_file, "\n")
cat("  flow column :", flow_col, "\n\n")

# ---- 3. Load the data ----
df <- read_csv(input_file, show_col_types = FALSE)

# Make sure Date is a real date
df <- df %>% mutate(Date = as.Date(Date))

# ---- 4. Add grouping columns based on period_type ----
if (period_type == "yearly") {
  df <- df %>% mutate(Year = year(Date))
  group_vars <- c("Station", "Longitude", "Latitude", "Year")
} else {
  df <- df %>% mutate(Year = year(Date), Month = month(Date))
  group_vars <- c("Station", "Longitude", "Latitude", "Year", "Month")
}

# ---- 5. Compute block max + the exact date it occurred ----
# slice_max keeps the row(s) where the flow column hits its max within each group.
# with_ties = FALSE -> if there's a tie, keep only the first occurrence.
block_max <- df %>%
  group_by(across(all_of(group_vars))) %>%
  slice_max(order_by = .data[[flow_col]], n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(MaxFlow = !!flow_col,
         MaxDate = Date) %>%
  select(all_of(group_vars), MaxDate, MaxFlow) %>%
  arrange(Station, MaxDate)

cat("Rows in block-max output:", nrow(block_max), "\n")
print(head(block_max))

# ---- 6. Save to a clearly-named file ----
output_file <- paste0("sf_", period_type, "_max_", model_type, ".csv")
write_csv(block_max, output_file)

cat("\nDone! Saved to:", output_file, "\n")

#check the output file 
head(read_csv(output_file))
