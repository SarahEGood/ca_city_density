library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(rgdal)

ui <- fluidPage(
  leafletOutput('map')
)
server <- function(input, output, session) {
  
  # Read in data
  df <- read.csv('main_data.csv')
  places_map <- readOGR(dsn = 'shapefile/City_Boundaries.shp', layer = 'City_Boundaries', GDAL1_integer64_policy = TRUE)
  projection <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
  places_map <- sp::spTransform(places_map,projection)
  
  # California data
  ca_outline <- readOGR(dsn = 'shapefile/cb_2018_us_state_500k.shp', layer = "cb_2018_us_state_500k", GDAL1_integer64_policy = TRUE)
  ca_outline <- ca_outline[ca_outline$NAME == "California",]
  
  # Merge Population Data
  df_small <- df[df$Year == 2021,]
  #places_map@data <- df_small %>% right_join(places_map@data, by = c("CITY"="CITY"))
  
  # Render places map
  ca_map <- ggplot() + geom_polygon(data = df_small, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
  
  output$map <- renderLeaflet({
    leaflet(places_map) %>%
      addTiles() %>%
      #addPolygons(data=ca_outline, fillColor='red', opacity = 1.0, fillOpacity = 0.5) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5)
  })
}
shinyApp(ui, server)