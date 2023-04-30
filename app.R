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
  places_map <- merge(places_map, df_small, by = "CITY", all.x=TRUE)
  
  # Create area palette
  places_palette <- colorNumeric(
    palette = c('lightblue', 'maroon'),
    domain = places_map$Total_Population
  )
  
  # Create area labels
  labels <- sprintf(
      "<strong>%s</strong><br/>%g people",
      places_map$CITY, places_map$Total_Population
    ) %>% lapply(htmltools::HTML)
  
  output$map <- renderLeaflet({
    leaflet(places_map) %>%
      addTiles() %>%
      addPolygons(color = "#444444",
                  weight = 0.25,
                  smoothFactor = 0.2,
                  opacity = 1.0,
                  fillOpacity = 0.75,
                  fillColor = ~places_palette(places_map$Total_Population),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
                  ) %>%
      addLegend(pal = places_palette, values = ~places_map$Total_Population, opacity = 1,
                title = "Population Percentile") #%>%
      #addPolygons(data=ca_outline, color = '#444444', weight = 3, opacity = 1.0, fillOpacity = 0)
  })
}
shinyApp(ui, server)