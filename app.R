library(shiny)
ui <- fluidPage(
  tableOutput('table')
)
server <- function(input, output, session) {
  df <- read.csv('main_data.csv')
  map <- readOGR('shapefile/City_Boundaries.shp')
  map2 <- readOGR('shapefile/cb_2018_us_state_500k.shp')
  output$table <- renderTable(df)
}
shinyApp(ui, server)