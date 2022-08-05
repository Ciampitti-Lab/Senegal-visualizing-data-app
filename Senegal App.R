#################################################################
# Author: SANTIAGO, Gustavo N.                                  #
# Coauthors: CARCEDO, Ana; CORRENDO, Adrian; CIAMPITTI, Ignacio #
#################################################################

# Loading R packages
library(shiny)
library(shinythemes)
library(leaflet)
library(scales)
library(rgdal)
library(dplyr)
library(BAMMtools)
library(htmltools)

###### Reading ShapeFile of Senegal's administrative districts
places <- readOGR("C:/Users/gusta/Desktop/K-State/Senegal visualizing data app/zonal_stats.shp")

##### Reading Data
placeBase <- read.csv("C:/Users/gusta/Desktop/K-State/Senegal visualizing data app/Test.csv", sep = ";", dec = ",")
showoptions <- names(subset(placeBase, select = -c(1,2)))

# Define User Interface function (UI)
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  theme = "united",
                  "Senegal Ag Data Comparisons",
                  tabPanel("Visualization",
                           #Sidebar Panel
                           sidebarPanel(
                             
                             # Place for select to insert which data will be visualized
                             tags$h4("Select what data you want to visualize:"),
                             varSelectInput("select",
                                         " ",
                                         subset(placeBase,select = -c(1,2))),
                             
                             # Place where users chose which year they want
                             tags$h4("Choose an year:"),
                             sliderInput("slider",
                                         " ",
                                         min = min(placeBase$Year),
                                         max = max(placeBase$Year),
                                         value = min(placeBase$Year),
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
server <- function(input, output, session) {
  
  
  ###### Filtering data
  dataInput <- reactive({
    placeBase %>%
      filter(Year == input$slider)%>%
      group_by(District)
  })
  
  choice <- reactive({
    input$select
  })
  
  
  ####### Adding palette of colors
  palette <- reactive({
    colorBin("RdYlBu", domain = dataInput()$Rain, bins = getJenksBreaks(dataInput()$Rain, 5))
  })
  
  ###### Creating the information that appears when of mouse-over
  labels <- reactive({
    paste("<p>", dataInput()$District, "<p>",
          "<p>", round(dataInput()$Rain, digits = 2), "<p>",
          sep ="")
  })
  
  ###### Creating the map
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -14.4524, lat = 14.4974 , zoom = 7) %>%
      addPolygons( data = places,
                   weight = 1,
                   smoothFactor = 0.5,
                   fillOpacity = 0.8,
                   color = ~palette()(dataInput()$Rain),
                   highlight = highlightOptions(
                     weight = 5,
                     color = "#666666",
                     fillOpacity = 0.7,
                     bringToFront = TRUE
                   ),
                   label = lapply(labels(), HTML)
                   ) %>%
      addLegend(pal = palette(),
                values = dataInput()$Rain,
                opacity = 0.7,
                position = "bottomright")
  })
  
}


# Creating Shiny object
shinyApp(ui = ui, server = server)