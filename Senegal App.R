#################################################################
# Author: SANTIAGO, Gustavo N.                                  #
# Coauthors: CARCEDO, Ana; CORRENDO, Adrian; CIAMPITTI, Ignacio #
#################################################################

# Loading R packages
library(shiny)
library(shinythemes)
library(leaflet)
library(scales)
library(plotly)
library(rgdal)
library(dplyr)
library(BAMMtools)
library(htmltools)

###### Reading ShapeFile of Senegal's administrative districts
districts <- readOGR("C:/Users/gusta/Desktop/K-State/Senegal visualizing data app/zonal_stats.shp")

##### Reading Data
senegalBase <- read.csv("C:/Users/gusta/Desktop/K-State/Senegal visualizing data app/Test.csv", sep = ";", dec = ",")

# Define User Interface function (UI)
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  theme = "united",
                  "Senegal Ag Data Comparisons",
                  tabPanel("Visualization",
                           #Sidebar Panel
                           sidebarPanel(
                             tags$h4("Select which data you want to visualize:"),
                             
                             # Place for radio check to insert which data will be visualized
                             radioButtons("dist", " ",
                                          c("Precipitation" = "precip",
                                            "Max. Temperature" = "maxtemp",
                                            "Min. Temperature" = "mintemp",
                                            "Evapotranspiration" = "evap")),
                             
                             # Place where users chose which year they want
                             tags$h4("Choose an year:"),
                             sliderInput("slider",
                                         " ",
                                         min = min(senegalBase$Year),
                                         max = max(senegalBase$Year),
                                         value = min(senegalBase$Year),
                                         step = 1)
                             
                           ),
                           
                           #Main Panel
                           mainPanel(
                             leafletOutput("mymap", height = 600),
                           ),
                           
                  ),
                  
                  tabPanel("Data", " *** Show all data of districts throught all years ***"),
                  
                  tabPanel("About", " *** Write about the project, consortion, Ciampitti Lab and authors ***"),
                  
                )
)


# Define server function  
server <- function(input, output) {
  
  
  ###### Filtering data
  data_input <- reactive({
    senegalBase %>%
      filter(Year == input$slider)%>%
      group_by(District)
  })
  
  
  ###### Aligning Data with Shapefile
  # data_input_ordered <- reactive({
  #   data_input()[order(match(data_input()$District, districts$DISTRICT))]
  # })
  
  
  palette <- reactive({
    colorBin("RdYlBu", domain = data_input()$Rain, bins = getJenksBreaks(data_input()$Rain, 5))
  })
  
  labels <- reactive({
    paste("<p>", data_input()$District, "<p>",
          "<p>", round(data_input()$Rain, digits = 2), "<p>",
          sep ="")
  })
  
  ###### Creating the map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -14.4524, lat = 14.4974 , zoom = 7) %>%
      addPolygons( data = districts,
                   weight = 1,
                   smoothFactor = 0.5,
                   color = "white",
                   fillOpacity = 0.8,
                   fillColor = ~palette(data_input()$Rain), ##### Lacking colorize map, solve here
                   highlight = highlightOptions(
                     weight = 5,
                     color = "#666666",
                     fillOpacity = 0.7,
                     bringToFront = TRUE
                   ),
                   label = lapply(labels(), HTML)
                   ) %>%
      addLegend(pal = palette(),
                values = data_input()$Rain,
                opacity = 0.7,
                position = "bottomright")
  })
  
}


# Creating Shiny object
shinyApp(ui = ui, server = server)