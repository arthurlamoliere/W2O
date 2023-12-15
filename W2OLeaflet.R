library(leaflet)
library(sf)
library(leaflet.extras)
library(leaflet.extras2)
library(sp)
library(raster)
library(hexbin)
library(htmlwidgets)
library(dplyr)

PA_shp <- st_read("path/to/Terrestrial Protected Areas Malta.shp")
points_data <- read.csv("path/to/W2O.csv")
points_sf <- st_read("path/to/W2O.shp")
folder_path <- "path/to/Species"

# List all shapefiles in the folder
shp_files <- list.files(folder_path, pattern = "\\.shp$", full.names = TRUE)

# Extract base names (without extension) for layer names
layer_names <- gsub("\\.shp$", "", basename(shp_files))

# Read shapefiles, convert to sf objects, and transform to WGS84
sf_list <- lapply(shp_files, function(shp) {
  layer <- st_read(shp)
  st_transform(layer, crs = 4326)  # Transform to WGS84
})

# Function to generate a range of diverse pastel colors
generate_pastel_colors <- function(n) {
  colors <- colorRampPalette(c(
    "#FFD1DC",  # Light Pink
    "#FFB6C1",  # Brighter Pink
    "#FFC0CB",  # Classic Pastel Pink
    "#B6E3FF",  # Light Blue
    "#CDB6FF",  # Light Purple
    "#FFFACD",  # Lemon Chiffon
    "#B0E0E6",  # Powder Blue
    "#FDB6C1",  # Pastel Orange
    "#E6E6FA",  # Lavender
    "#FFFFE0"   # Light Yellow
  ))(n)
  return(colors)
}

# Make sure to use the correct function when assigning colors to shapefile layers
colors <- generate_pastel_colors(length(sf_list))

# Create the Leaflet map
map <- leaflet(points_sf) %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "OSM Dark Matter") %>%
  addHeatmap(
    lng = ~Longitude, lat = ~Latitude, 
    blur = 50, max = 0.1, radius = 30,
    group = "Heatmap"
  ) %>%
  addPolygons(
    data = PA_shp,
    fillColor = "transparent",
    color = "green",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.5,
    group = "Terrestrial Protected Areas"
  ) %>%
  addWMSTiles(
    baseUrl = "https://bio.discomap.eea.europa.eu/arcgis/services/Ecosystem/EcosystemTypeMap_v3_1_Terrestrial/MapServer/WMSServer?",
    layers = "1",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "European Environment Agency",
    group = "Ecosystem Type Map"
  )

# Add each shapefile as a polygon layer to the map
for (i in seq_along(sf_list)) {
  map <- map %>%
    addPolygons(data = sf_list[[i]], 
                color = colors[i], 
                fillOpacity = 0.5, 
                group = layer_names[i],
                popup = ~paste0("<img src='", occurrence, 
                                "' style='width:100px;'><br>",
                                "Scientific name: ", Scientific, 
                                "<br>Date: ", Date))
}

# Add Layers Control after all layers are added
map <- map %>%
  addLayersControl(
    baseGroups = c("OSM Dark Matter"),
    overlayGroups = c("Heatmap", "Terrestrial Protected Areas", "Ecosystem Type Map", layer_names),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addScaleBar()

htmlContent <- paste(
  '<div id="mapTitle" style="position: absolute; bottom: 10px; left: 50px; color: white;">',
  '<img src="https://w2o.s3.eu-north-1.amazonaws.com/W2Ologo.png" style="height: 200px;"/>',
  '<h1>Wild Orchids Observatory</h1>',
  '<h2>A Citizen Science Observatory of the wild Orchids in the Maltese Islands</h2>',
  '</div>'
)

# Inject HTML into the map
map <- map %>% onRender(
  sprintf("function(el) { $(el).append('%s'); }", htmlContent)
)

# Print the map
print(map)

#export in html
saveWidget(map, file = "W2Omap.html")













