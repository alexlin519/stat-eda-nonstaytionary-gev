library(terra)
library(sf)
library(leaflet)
library(readxl)
library(dplyr)
library(htmlwidgets) # Needed for saving the HTML files

# 1. Setup & Read Data 
# (Make sure these point to your desired dataset, e.g., History)
yearly_val <- rast("filtered_yearly_max_val_History_1945_2012.tif")
yearly_doy <- rast("filtered_yearly_max_doy_History_1945_2012.tif")
monthly_val <- rast("filtered_monthly_max_val_History_1945_2012.tif")

coords_df <- read_excel("station_coordinate_apr15.xlsx")
stations_sf <- st_as_sf(coords_df, coords = c("Longitude", "Latitude"), crs = 4326)

# Create a dedicated folder for the output maps
dir.create("Interactive_Maps", showWarnings = FALSE)

# 2. Setup Loop Parameters
start_year <- 1945
num_years <- nlyr(yearly_val)

# If you only want to test the first 2 years, change num_years to 2.
for (yr_idx in 1:num_years) {
  current_year <- start_year + yr_idx - 1
  print(paste("Generating Map for:", current_year, "..."))
  
  # --- Extract exactly what we need for this specific year ---
  y_val_layer <- yearly_val[[yr_idx]]
  y_doy_layer <- yearly_doy[[yr_idx]]
  
  # Monthly math: Year 1 is layers 1-12. Year 2 is 13-24, etc.
  m_start <- (yr_idx - 1) * 12 + 1
  m_end <- m_start + 11
  m_val_layers <- monthly_val[[m_start:m_end]]
  names(m_val_layers) <- month.abb 
  
  # Combine into one stack for this specific year
  map_stack <- c(y_val_layer, y_doy_layer, m_val_layers)
  names(map_stack)[1:2] <- c("Max_Baseflow", "Day_of_Year")
  
  grid_boundaries <- as.polygons(map_stack, aggregate = FALSE)
  grid_sf <- st_as_sf(grid_boundaries)
  
  # --- Build Popups & Calculate Opacity ---
  popup_list <- character(nrow(grid_sf))
  
  for (i in 1:nrow(grid_sf)) {
    row_data <- grid_sf[i, ]
    y_max <- row_data$Max_Baseflow
    doy <- row_data$Day_of_Year
    
    if (is.na(y_max)) {
      popup_list[i] <- "No data"
      next
    }
    
    m_vals <- as.numeric(st_drop_geometry(row_data)[, month.abb])
    local_max <- max(m_vals, na.rm = TRUE)
    if (local_max == 0 || is.na(local_max)) local_max <- 1 
    
    peak_month_idx <- which.max(m_vals)
    
    # Precise y-axis ticks
    tick_1 <- sprintf("%.2f", local_max)
    tick_2 <- sprintf("%.2f", local_max * 0.75)
    tick_3 <- sprintf("%.2f", local_max * 0.50)
    tick_4 <- sprintf("%.2f", local_max * 0.25)
    tick_5 <- sprintf("%.2f", 0)
    
    bars_html <- ""
    for (m in 1:12) {
      val <- m_vals[m]
      if (is.na(val)) val <- 0
      ht_pct <- round((val / local_max) * 100)
      bar_color <- ifelse(m == peak_month_idx, "red", "steelblue")
      bars_html <- paste0(bars_html,
                          "<div style='display:inline-block; width:12px; height:100%; margin-right:2px; position:relative;'>",
                          "<div title='", month.abb[m], ": ", sprintf("%.2f", val), "' ",
                          "style='position:absolute; bottom:0; width:100%; height:", ht_pct, "%; background-color:", bar_color, ";'></div>",
                          "</div>"
      )
    }
    
    readable_date <- format(as.Date(doy - 1, origin = paste0(current_year, "-01-01")), "%B %d, %Y")
    
    popup_list[i] <- paste0(
      "<div style='font-family: Arial; width:240px;'>",
      "<h4 style='margin-bottom: 5px; margin-top: 0;'>", current_year, " Maximum</h4>",
      "Baseflow: ", sprintf("%.2f", y_max), "<br>",
      "Date: ", readable_date, "<br><br>",
      "<div style='font-size:12px; font-weight:bold; margin-bottom:2px;'>Monthly Maximums</div>",
      "<div style='font-size:10px; color:#555; margin-bottom:5px;'>(Red bar = Annual Max)</div>",
      "<div style='display:flex; height:100px;'>",
      "<div style='display:flex; flex-direction:column; justify-content:space-between; font-size:9px; color:#666; padding-right:5px; border-right:1px solid #333; text-align:right; width:45px;'>",
      "<span>", tick_1, "</span><span>", tick_2, "</span><span>", tick_3, "</span><span>", tick_4, "</span><span>", tick_5, "</span>",
      "</div>",
      "<div style='display:flex; align-items:flex-end; padding-left:5px; height:100%;'>", bars_html, "</div>",
      "</div>",
      "<div style='display:flex; justify-content:space-between; font-size:9px; color:#666; margin-top:2px; margin-left:50px;'>",
      "<span>Jan</span><span>Dec</span>",
      "</div></div>"
    )
  }
  
  grid_sf$Popup_HTML <- popup_list
  grid_sf$Relative_Opacity <- 0.2 + 0.7 * (grid_sf$Max_Baseflow / max(grid_sf$Max_Baseflow, na.rm = TRUE))
  
  # --- Define Palettes & Generate Map ---
  pal_mag <- colorNumeric(palette = "YlOrRd", domain = grid_sf$Max_Baseflow, na.color = "transparent")
  pal_season <- colorNumeric(palette = "Spectral", domain = c(1, 365), na.color = "transparent", reverse = TRUE)
  
  current_map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = grid_sf, fillColor = ~pal_season(Day_of_Year), fillOpacity = ~Relative_Opacity, color = "#333", weight = 0.5, opacity = 0.8, group = "Timing of Annual Peak", popup = ~Popup_HTML) %>%
    addPolygons(data = grid_sf, fillColor = ~pal_mag(Max_Baseflow), fillOpacity = 0.8, color = "#333", weight = 0.5, opacity = 0.8, group = "Baseflow Volume Heatmap", popup = ~Popup_HTML) %>%
    addCircleMarkers(data = stations_sf, color = "#333", fillColor = "white", fillOpacity = 1, radius = 5, weight = 2, group = "Stations", label = ~paste0("Station: ", `Location Code`), labelOptions = labelOptions(style = list("padding" = "3px 8px"), textsize = "13px", direction = "auto")) %>%
    addLayersControl(baseGroups = c("Timing of Annual Peak", "Baseflow Volume Heatmap"), overlayGroups = c("Stations"), options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend(pal = pal_mag, values = grid_sf$Max_Baseflow, title = "Max Baseflow (Volume)", position = "bottomright", group = "Baseflow Volume Heatmap") %>%
    addLegend(colors = pal_season(c(15, 105, 196, 288, 350)), labels = c("January", "April", "July", "October", "December"), title = "Peak Month (Timing)", position = "bottomleft", group = "Timing of Annual Peak")
  
  # --- Save the map directly to the folder ---
  filename <- paste0("Interactive_Maps/Baseflow_Map_", current_year, ".html")
  saveWidget(current_map, file = filename, selfcontained = TRUE)
}

print("All maps generated successfully!")