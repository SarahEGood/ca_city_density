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
  
  # Apply projection
  ca_outline <- sp::spTransform(ca_outline,projection)
  
  # Merge Population Data
  df_small <- df[df$Year == 2021,]
  #places_map@data <- df_small %>% right_join(places_map@data, by = c("CITY"="CITY"))
  
  
  output$map <- renderLeaflet({
    leaflet(places_map) %>%
      addTiles() %>%
      addPolygons(color = "#444444", fillColor = 'blue', weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5) %>%
      addPolygons(data=ca_outline, color = '#444444', weight = 3, opacity = 1.0, fillOpacity = 0)
  })
}
shinyApp(ui, server)