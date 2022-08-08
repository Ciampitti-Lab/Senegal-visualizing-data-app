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

# Define User Interface function (UI)
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  theme = "united",
                  "Senegal Ag Data Comparisons",
                  tabPanel("Map visualization",
                           #Sidebar Panel
                           sidebarPanel(
                             
                             # Place for select to insert which data will be visualized
                             tags$h4("Select what data you want to visualize:"),
                             varSelectInput("select",
                                         " ",
                                         subset(placeBase,select = -c(1,2))),
                             
                             # Place where users chose which year they want
                             tags$h4("Choose a year:"),
                             sliderInput("slider",
                                         " ",
                                         min = min(placeBase$Year),
                                         max = max(placeBase$Year),
                                         value = min(placeBase$Year),
                                         step = 1),
                             radioInput("mean", " ", c("Mean" = "yearMean"))
                             
                           ),
                           
                           #Main Panel
                           mainPanel(
                             leafletOutput("mymap", height = 600),
                           ),
                           
                  ),
                  
                  tabPanel("Graph Plot Visualization", " *** Plot radar graph here ***"),
                  
                  tabPanel("Data", 
                           #Main Panel
                           mainPanel(
                             tags$h2("This is the data you have uploaded: "),
                             dataTableOutput("dataInput")
                           )),
                  
                  tabPanel("About",
                           tags$h1("About the project:"),
                           tags$h5("The process of assessing the impact of agricultural interventions is a key point when
deciding on future research investments and new policies. This task requires knowledge of the
chain of reactions in multiple realms that occur after an intervention. Filling this critical gap,
USAID's Feed the Future Innovation Lab for Collaborative Research in Sustainable
Intensification has developed the Sustainable Intensification Assessment Framework (SIAF) to
assess the impact of interventions considering five domains (productivity, economic,
environmental, human and social). Until recently, however, there has been a lack of integration
across the domains relative to the evaluation of agricultural interventions for different regions
around the globe."),
                           tags$h5("A recent review executed by our team identified almost no studies addressing all five
domains of the SIAF simultaneously, emphasizing this lack of integration. Many challenges that
result from this issue include data isolation (relevant data in multiple individual databases), scale
(data with different spatial and temporal scales), and integrity (lack of complete datasets).
Therefore, we propose to design an interactive dashboard to aggregate and summarize the
available open-access data layers using the SIAF as a platform to facilitate the visualization of
the domain interactions. Furthermore, we seek to create a unified data resource to enable
integration and comparison across both spatial and temporal scales."),
                           tags$h5("This platform targets the needs of donor-investment, policymakers, researchers and on-
ground extension-specialists to support planned interventions. Lastly, this platform will serve as
an example for the application of the SIAF not only for Senegal but for multiple regions around
the world."),
                           tags$h2("Authors: "),
                           "Ana J.P. Carcedo, Molly E. Brown, Jason Neff, Kathryn Grace, Paul West, James Gerber, A.
Pouyan Nejadhashemi, Ignacio A. Ciampitti and Gustavo N. Santiago."
                           
                           ),
                           
                  
                )
)


# Define server function  
server <- function(input, output, session) {
  
  ####### Filtering data
  dataInput <- reactive({
    placeBase %>%
      filter(Year == input$slider) %>%
      group_by(District) %>%
      select(dataUsed = input$select)
  })
  
  ####### Adding palette of colors
  palette <- reactive({
    colorNumeric("RdYlBu", domain = dataInput()$dataUsed)
  })
  
  ####### Creating the information that appears when of mouse-over
  labels <- reactive({
    paste("<p>", dataInput()$District, "<p>",
          "<p>", round(dataInput()$dataUsed, digits = 2), "<p>",
          sep ="")
  })
  
  ####### Creating the map
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -14.4524, lat = 14.4974 , zoom = 7) %>%
      addPolygons( data = places,
                   weight = 1,
                   smoothFactor = 0.5,
                   fillOpacity = 0.8,
                   color = ~palette()(dataInput()$dataUsed),
                   highlight = highlightOptions(
                     weight = 5,
                     color = "#666666",
                     fillOpacity = 0.7,
                     bringToFront = TRUE
                   ),
                   label = lapply(labels(), HTML)
                   ) %>%
      addLegend(pal = palette(),
                values = dataInput()$dataUsed,
                opacity = 0.7,
                position = "bottomright")
  })
  
  ####### Summarizing data input
  output$dataInput <- renderDataTable(placeBase)
  
}


# Creating Shiny object
shinyApp(ui = ui, server = server)