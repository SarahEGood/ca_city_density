library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(rgdal)
library(raster)

ui <- fluidPage(
  titlePanel("California Population Density (by City)"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput('distance_units',
                     label = "Square Distance Measure",
                     choices=c("Square Miles" = "pop_sqmi", "Square Kilometers" = "pop_sqkm"),
                     selected = "Square Miles"),
      selectizeInput('year',
                     label = "Year",
                     choices= seq(2010, 2021, by=1),
                     selected = 2021),
            width = 4),
    
    mainPanel(
      tags$style(type = "text/css", "#map {height: calc(85vh) !important;}"),
      leafletOutput('map')
    )
  )
)

server <- function(input, output, session) {
  
  # Read in Population Data
  df <- read.csv('main_data.csv')
  
  # Read in and transform projection for CA places
  places_map <- readOGR(dsn = 'shapefile/City_Boundaries.shp', layer = 'City_Boundaries', GDAL1_integer64_policy = TRUE)
  
  projection <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
  places_map <- sp::spTransform(places_map,projection)
  
  # Get area of places (square km and square miles)
  
  places_map$area_sqkm <- area(places_map) / 1000000
  places_map$area_sqmi <- 0.386102 * places_map$area_sqkm
  
  # Track changes in input values
  changable_vals <- reactive({
    list(input$distance_units, input$year)
    })
  
  observeEvent(changable_vals(), {
    
    # Filter to relevant year
    #places_map_slice <- places_map[places_map$Year == input$year,]
    
    # Merge Population Data
    df_small <- df[df$Year == input$year,]
    places_map_slice <- merge(places_map, df_small, by = "CITY", all.x=TRUE)
    
    # Calc respective population densities
    places_map_slice$pop_sqkm <- places_map_slice$Total_Population / places_map_slice$area_sqkm
    places_map_slice$pop_sqmi <- places_map_slice$Total_Population / places_map_slice$area_sqmi
    
    # Get distance measure
    choices_distance <- c("Sq Mi" = "pop_sqmi", "Sq Km" = "pop_sqkm")
    
    distance_units <- places_map_slice[[input$distance_units]]
    distance_name <- names(choices_distance)[choices_distance == input$distance_units]
    print(paste0(input$distance_units))
    print(distance_name)
    
    # Create area palette
    places_palette <- colorNumeric(
      palette = c('lightblue', 'maroon'),
      domain = distance_units
    )
    
    # Create area labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Population / %s",
      places_map_slice$CITY, places_map_slice[[input$distance_units]], distance_name
    ) %>% lapply(htmltools::HTML)
    print(labels)
    
    output$map <- renderLeaflet({
      leaflet(places_map_slice) %>%
        addTiles() %>%
        addPolygons(color = "#444444",
                    weight = 0.25,
                    smoothFactor = 0.2,
                    opacity = 1.0,
                    fillOpacity = 0.75,
                    fillColor = ~places_palette(distance_units),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
                    ) %>%
        addLegend(pal = places_palette, values = ~distance_units, opacity = 1,
                  title = paste0("Population / ", distance_name))
    })
    
  })
  
}
shinyApp(ui, server)