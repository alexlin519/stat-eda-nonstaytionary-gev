library(terra)
library(sf)
library(leaflet)
library(readxl)
library(dplyr)

# 1. Setup Data for a Single Year
target_year <- 1945

# Read station coordinates
coords_df <- read_excel("station_coordinate_apr15.xlsx")
stations_sf <- st_as_sf(coords_df, coords = c("Longitude", "Latitude"), crs = 4326)

# Read Yearly Data (Updated to Prep files)
yearly_val <- rast("filtered_station_yearly_max_3day_prep_1945_2012.tif")[[1]]
yearly_doy <- rast("filtered_station_yearly_max_doy_3day_prep_1945_2012.tif")[[1]]

# Read Monthly Data (Updated to Prep files)
monthly_val <- rast("filtered_station_monthly_max_3day_prep_1945_2012.tif")[[1:12]]
names(monthly_val) <- month.abb 

# Combine and convert to polygons
map_stack <- c(yearly_val, yearly_doy, monthly_val)
names(map_stack)[1:2] <- c("Max_Prep", "Day_of_Year") # Changed to Max_Prep
grid_boundaries <- as.polygons(map_stack, aggregate = FALSE)
grid_sf <- st_as_sf(grid_boundaries)

# 2. Pre-calculate the Upgraded Mini-Charts
popup_list <- character(nrow(grid_sf))

for (i in 1:nrow(grid_sf)) {
  row_data <- grid_sf[i, ]
  y_max <- row_data$Max_Prep # Changed to Max_Prep
  doy <- row_data$Day_of_Year
  
  if (is.na(y_max)) {
    popup_list[i] <- "No data"
    next
  }
  
  m_vals <- as.numeric(st_drop_geometry(row_data)[, month.abb])
  local_max <- max(m_vals, na.rm = TRUE)
  if (local_max == 0 || is.na(local_max)) local_max <- 1 
  
  peak_month_idx <- which.max(m_vals)
  
  # Build the bars
  bars_html <- ""
  for (m in 1:12) {
    val <- m_vals[m]
    if (is.na(val)) val <- 0
    
    ht_pct <- round((val / local_max) * 100)
    bar_color <- ifelse(m == peak_month_idx, "red", "steelblue")
    
    bars_html <- paste0(bars_html,
                        "<div style='display:inline-block; width:12px; height:100%; margin-right:2px; position:relative;'>",
                        "<div title='", month.abb[m], ": ", round(val, 1), "' ",
                        "style='position:absolute; bottom:0; width:100%; height:", ht_pct, "%; background-color:", bar_color, ";'></div>",
                        "</div>"
    )
  }
  
  readable_date <- format(as.Date(doy - 1, origin = paste0(target_year, "-01-01")), "%B %d, %Y")
  
  # Assemble the newly formatted HTML popup (Updated text to Precipitation)
  popup_list[i] <- paste0(
    "<div style='font-family: Arial; width:220px;'>",
    "<h4 style='margin-bottom: 5px; margin-top: 0;'>", target_year, " Maximum</h4>",
    "Max 3-Day Prep: ", round(y_max, 2), "<br>",
    "Date: ", readable_date, "<br><br>",
    
    # New Chart Titles
    "<div style='font-size:12px; font-weight:bold; margin-bottom:2px;'>Monthly Maximums</div>",
    "<div style='font-size:10px; color:#555; margin-bottom:5px;'>(Red bar = Annual Max)</div>",
    
    # CSS Grid for Y-Axis and Bars
    "<div style='display:flex; height:80px;'>",
    
    # Y-Axis Column
    "<div style='display:flex; flex-direction:column; justify-content:space-between; font-size:9px; color:#666; padding-right:5px; border-right:1px solid #333; text-align:right; width:35px;'>",
    "<span>", round(local_max, 0), "</span>",
    "<span>", round(local_max / 2, 0), "</span>",
    "<span>0</span>",
    "</div>",
    
    # Bar Chart Column
    "<div style='display:flex; align-items:flex-end; padding-left:5px; height:100%;'>",
    bars_html,
    "</div>",
    
    "</div>",
    
    # X-Axis Labels (Jan to Dec)
    "<div style='display:flex; justify-content:space-between; font-size:9px; color:#666; margin-top:2px; margin-left:40px;'>",
    "<span>Jan</span><span>Dec</span>",
    "</div>",
    
    "</div>"
  )
}

grid_sf$Popup_HTML <- popup_list
grid_sf$Relative_Opacity <- 0.4 + 0.7 * (grid_sf$Max_Prep / max(grid_sf$Max_Prep, na.rm = TRUE))

# 3. Define Palettes (Using Max_Prep)
pal_mag <- colorNumeric(palette = "YlOrRd", domain = grid_sf$Max_Prep, na.color = "transparent")
pal_season <- colorNumeric(
  palette = colorRamp(c("#1E90FF", "#FF0000", "#1E90FF"), interpolate = "linear"),
  domain = c(1, 365)
)

# 4. Generate the Map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Layer 1: Timing of Annual Peak
  addPolygons(
    data = grid_sf,
    fillColor = ~pal_season(Day_of_Year),
    fillOpacity = ~Relative_Opacity, 
    color = "#333", weight = 0.5, opacity = 0.8,
    group = "Timing of Annual Peak",
    popup = ~Popup_HTML
  ) %>%
  
  # Layer 2: Precipitation Volume Heatmap
  addPolygons(
    data = grid_sf,
    fillColor = ~pal_mag(Max_Prep),
    fillOpacity = 0.8,
    color = "#333", weight = 0.5, opacity = 0.8,
    group = "Precipitation Volume Heatmap",
    popup = ~Popup_HTML
  ) %>%
  
  # Enhanced Stations
  addCircleMarkers(
    data = stations_sf,
    color = "#333",        
    fillColor = "white",     
    fillOpacity = 1,
    radius = 5,              
    weight = 2,
    group = "Stations",
    label = ~paste0("Station: ", `Location Code`, " | Lon: ", round(st_coordinates(stations_sf)[,1], 2), ", Lat: ", round(st_coordinates(stations_sf)[,2], 2)),
    labelOptions = labelOptions(style = list("padding" = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  
  # Controls with Clearer Names
  addLayersControl(
    baseGroups = c("Timing of Annual Peak", "Precipitation Volume Heatmap"),
    overlayGroups = c("Stations"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Clearer Legend 1
  addLegend(
    pal = pal_mag, 
    values = grid_sf$Max_Prep, 
    title = "Max 3-Day Prep", 
    position = "bottomright",
    group = "Precipitation Volume Heatmap"
  ) %>%
  
  # Clearer Legend 2
  addLegend(
    colors = pal_season(c(15, 105, 196, 288, 350)),
    labels = c("January", "April", "July", "October", "December"),
    title = "Peak Month (Timing)",
    position = "bottomleft",
    group = "Timing of Annual Peak"
  )
