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


# Define User Interface function (UI)
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  theme = "united",
                  "Senegal Ag Data Comparisons",
                  tabPanel("Visualization",
                           #Sidebar Panel
                           sidebarPanel(
                             tags$h4("Select which data you want to visualize:"),
                             
                             # Place for checkbox to insert which data will be visualized
                             radioButtons("dist", " ",
                                          c("Precipitation" = "precip",
                                            "Max. Temperature" = "maxtemp",
                                            "Min. Temperature" = "mintemp",
                                            "Evapotranspiration" = "evap")),
                             
                             # Place where users chose which year they want
                             tags$h4("Choose an year:"),
                             sliderInput("year", " ", 2015, 2021, 2015, 1),
                             
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
server <- function(input, output, session) {
  
  ###### Reading ShapeFile of Senegal's administrative districts
  districts <- readOGR("C:/Users/gusta/Desktop/K-State/Senegal visualizing data app/zonal_stats.shp")
  
  ##### Reading Data
  senegalInfo <- read.csv("C:/Users/gusta/Desktop/K-State/Senegal visualizing data app/test.csv", sep = ";")
  senegalData <- senegalInfo %>%
    group_by(District)%>%
    summarise(Num.Rain = sum(Rain))
  
  ###### Aliugning Data with Shapefile
  districts <- subset(districts, is.element(districts$DISTRICT, senegalData$District))
  senegalData <- senegalData[order(match(senegalData$District, districts$DISTRICT))]
  
  ###### Setting data visualization 
  datashown <- districts$RAIN
  palette <- colorBin("RdYlBu", bins = getJenksBreaks(datashown, 5))
  labels <- paste("<p>", districts$DISTRICT, "<p>",
                  "<p>", round(datashown, digits = 2), "<p>",
                  sep ="")
  
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
                   fillColor = ~palette(datashown),
                   highlight = highlightOptions(
                     weight = 5,
                     color = "#666666",
                     fillOpacity = 0.7,
                     bringToFront = TRUE
                   ),
                   label = lapply(labels, HTML)
                   ) %>%
      addLegend(pal = palette,
                values = datashown,
                opacity = 0.7,
                position = "bottomright")
  })
  
}


# Creating Shiny object
shinyApp(ui = ui, server = server)