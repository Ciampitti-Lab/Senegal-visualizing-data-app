#################################################################
# Author: SANTIAGO, Gustavo N.                                  #
# Coauthors: CARCEDO, Ana; CORRENDO, Adrian; CIAMPITTI, Ignacio #
#################################################################

# Loading R packages
library(shiny)
library(leaflet)
library(scales)
library(rgdal)
library(dplyr)
library(htmltools)
library(ggplot2)
library(RColorBrewer)
library(fmsb)
library(psych)
library(shapefiles)

####### Reading ShapeFile of Senegal's administrative districts
<<<<<<< Updated upstream
places <- readOGR("zonal_stats.shp")

####### Reading Data
placeBase <- read.csv("Test.csv", sep = ";", dec = ",")
=======
places <- readOGR("C:/Users/gusta/OneDrive/Documentos/K-State/Senegal visualizing data app/Senegal-visualizing-data-app/zonal_stats.shp")

####### Reading Data
placeBase <- read.csv("C:/Users/gusta/OneDrive/Documentos/K-State/Senegal visualizing data app/Senegal-visualizing-data-app/Teste.csv")
>>>>>>> Stashed changes

####### Define User Interface function (UI)
ui <- fluidPage(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                navbarPage(
                  theme = "united",
                  "Senegal Data Visualization",
                  tabPanel("Visualization",
                           ####### Sidebar Panel
                           sidebarPanel(
<<<<<<< Updated upstream

                             ####### Place for select to insert which data will be visualized
                             tags$h3("Select what data you want to visualize:"),
                             varSelectInput("select",
                                         " ",
                                         subset(placeBase,select = -c(1,2))),
                             
=======
                             conditionalPanel(condition = "input.tabselected==1",
                                              ####### Place for select to insert which data will be visualized in map
                                              uiOutput("select")
                             ),
                             conditionalPanel(condition = "input.tabselected==2",
                                              ####### Place for select to insert which graph you want to visualize
                                              uiOutput("selectgraph"),
                                              ####### Place for select to insert which data will be visualized in graphs
                                              uiOutput("selectfgraph"),
                                              ####### Place for select to insert which places they want to compare on graphs
                                              uiOutput("selectplaces")
                             ),
>>>>>>> Stashed changes
                             ####### Place where users chose which year they want
                             radioButtons("mean",
                                          "Period",
                                          c("Mean of years" = "yearMean",
                                            "Choose a year bellow" = "choose"
                                          )
                             ),
                             uiOutput("year")
                           ),
                           ####### Main Panel
                           mainPanel(
                             tabsetPanel (type = "tabs",
                                          id = "tabselected",
                                          tabPanel("Map",
                                                   value=1,
                                                   ####### Showing map
                                                   leafletOutput("mymap")
                                          ),
                                          tabPanel("Graphs",
                                                   value=2,
                                                   ####### Ploting graph
                                                   plotOutput("graph")
                                          )
                             )
                           )
                           
                  ),
                  tabPanel("Data", 
                           ####### Rendering the table of data user uploaded
<<<<<<< Updated upstream
                            tags$h2("This is the data you have uploaded: "),
                            dataTableOutput("dataInput")
                           ),
                  
                  tabPanel("About",
                           
                           ####### Text showing or what the project is about
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
    )
  )
=======
                           tags$h2("This is the data you have uploaded: "),
                           dataTableOutput("dataInput")
                  ),
                  tabPanel("About", 
                           includeHTML("About.html")
                  )
                )
>>>>>>> Stashed changes
)


# Define server function  
server <- function(input, output, session) {
  
  ####### Creating Dropdown for map
  output$select <- renderUI({
    varSelectInput("select",
                   "Data you want to visualize on map:",
                   subset(placeBase,select = -c(1,2)))
  })
  
  ####### Creating Dropdown for graph type
  output$selectgraph <- renderUI({
    selectInput("selectgraph",
                "Type of graph:",
                choices = c("Bar Graph" = "bar",
                            "Radar Graph" = "radar",
                            "Correlation Graph" = "cor",
                            "Line Graph" = "line")
    )
  })
  
  ####### Creating Dropdown for data to be ploted on graph
  output$selectfgraph <- renderUI({
    if(input$selectgraph == "radar"){
      varSelectInput("selectfgraph",
                     "Data you want to visualize on graph:",
                     subset(placeBase,select = -c(1,2)),
                     multiple = TRUE)
    }
    else if(input$selectgraph == "cor"){
      varSelectInput("selectfgraph",
                     "Data you want to visualize on graph:",
                     subset(placeBase,select = -c(1,2)),
                     multiple = TRUE)
    }
    else if(input$selectgraph == "bar"){
      varSelectInput("selectfgraph",
                     "Data you want to visualize on graph:",
                     subset(placeBase,select = -c(1,2)))
    }
    else if(input$selectgraph == "line"){
      varSelectInput("selectfgraph",
                     "Data you want to visualize on graph:",
                     subset(placeBase,select = -c(1,2)))
    }
  })
  
  ####### Creating Dropdown for places to plot on graph
  output$selectplaces <- renderUI({
    place <- toString(names(subset(placeBase, select = c(1))))
    choice <- placeBase %>%
      group_by(States = eval(parse(text = place)))
    
    if(input$selectgraph == "cor"){
      selectInput("selectplaces",
                  "Places you want to visualize on graph:",
                  choice$States)
    }
    else if (input$selectgraph != "cor"){
      selectInput("selectplaces",
                  "Places you want to visualize on graph:",
                  choice$States,
                  multiple = TRUE)
    }
  })
  
  ####### Creating Select for period
  output$year <- renderUI({
    year <- toString(names(subset(placeBase, select = c(2))))
    choice <- placeBase %>%
      group_by(Year = eval(parse(text = year)))
    
    selectInput("year",
                " ",
                choice$Year,
                selected = 1)
  })
  
  ####### Filtering data for map
  dataInput <- reactive({
    place <- toString(names(subset(placeBase, select = c(1))))
    period <- toString(names(subset(placeBase, select = c(2))))
    
    placeOrder <- unique(placeBase[,1])
    
    if(input$mean == "yearMean"){
      choice <- toString(input$select)
      placeBase %>%
        group_by(States = eval(parse(text = place))) %>%
        summarise(dataUsed = mean(eval(parse(text = choice)), na.rm=TRUE)) %>%
        arrange(factor(States, levels = placeOrder))
    }
    
    else if(input$mean == "choose"){
      placeBase %>%
        filter(eval(parse(text = period)) == input$year) %>%
        group_by(States = eval(parse(text = place))) %>%
        select(dataUsed = input$select)
    }
    
  })
  
  ####### Getting maximum and minimum values
  max_min <- reactive({
    
    choicesD <- as.character(input$selectfgraph)
    
    filtro <- subset(placeBase, select = -c(1,2))
    
    n_filtro <- filtro%>%
      select_at(vars(choicesD))
    
    max <- n_filtro %>%
      summarise_all(max, na.rm=TRUE)
    
    min <- n_filtro %>%
      summarise_all(min, na.rm=TRUE)
    
    max_min <- rbind(max, min)
    
    max_min <- data.frame(max_min, row.names = c("Max", "Min"))
    
  })
  
  ####### Filtering data for graph
  dataInputGraph <- reactive({
    place <- toString(names(subset(placeBase, select = c(1))))
    period <- toString(names(subset(placeBase, select = c(2))))
    
    if (input$selectgraph == "bar"){
      
      if(input$mean == "yearMean"){
        choiceG <- toString(input$selectfgraph)
        placeBase %>%
          filter(eval(parse(text = place)) %in% input$selectplaces) %>%
          group_by(Places = eval(parse(text = place))) %>%
          summarise(Data = mean(eval(parse(text = choiceG)), na.rm=TRUE))  
      }
      
      else if(input$mean == "choose"){
        placeBase %>%
          filter(eval(parse(text = period)) == input$year,
                 eval(parse(text = place)) %in% input$selectplaces) %>%
          group_by(Places = eval(parse(text = place))) %>%
          select(Data = input$selectfgraph)
      }
      
    }
    
    else if (input$selectgraph == "radar"){
      
      choicesD <- as.character(input$selectfgraph)
      
      if(input$mean == "yearMean"){
        placeBase %>%
          filter(eval(parse(text = place)) %in% input$selectplaces) %>%
          group_by(Places = eval(parse(text = place))) %>%
          summarise_at(vars(choicesD), mean, na.rm=TRUE)
      }
      
      else if(input$mean == "choose"){
        placeBase %>%
          filter(eval(parse(text = place)) %in% input$selectplaces,
                 eval(parse(text = period)) == input$year) %>%
          group_by(Places = eval(parse(text = place))) %>%
          select_at(vars(choicesD))
      }
      
    }
    
    else if (input$selectgraph == "cor"){
      
      choicesD <- as.character(input$selectfgraph)
      
      placeBase %>%
        filter(eval(parse(text = place)) %in% input$selectplaces) %>%
        group_by(Places = eval(parse(text = place))) %>%
        select_at(vars(choicesD))
      
    }
    
    else if(input$selectgraph == "line"){
      
      choicesD <- as.character(input$selectfgraph)
      
      placeBase %>%
        filter(eval(parse(text = place)) %in% input$selectplaces) %>%
        group_by(Year = eval(parse(text = period)),
                 Places = eval(parse(text = place))) %>%
        select(Data = choicesD)
    }
    
  })
  
  ####### Adding colors palettes
  palette <- reactive({
    colorNumeric("RdYlBu", domain = dataInput()$dataUsed)
  })
  
  ####### Creating the information that appears when mouse-over
  labels <- reactive({
    paste("<p>", dataInput()$States, "<p>",
          "<p>", round(dataInput()$dataUsed, digits = 2), "<p>",
          sep ="")
  })
  
  ####### Creating the map
  output$mymap <- renderLeaflet({
    
    leaflet(width = 200, height = 300) %>%
      addTiles() %>%
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
  
  ####### Creating the graph
  output$graph <- renderPlot({
    
    
    if (input$selectgraph == "bar"){
      ggplot(dataInputGraph(), aes(x=Places, y=Data))+
        geom_bar(stat="identity", color="#594433", fill="#9ba657")+
        geom_text(aes(label=round(Data, 2)), vjust=1.6, color="white", size=3.5)+
        theme(
          panel.background = element_rect(fill = "#f0e5c9",
                                          colour = "#f0e5c9",
                                          size = 0.5, linetype = "solid")
        )
    }
    
    else if(input$selectgraph == "radar"){
      
      if(nrow(dataInputGraph()) == 1){
        dataGraph <- dataInputGraph()
        rownames(dataGraph) <- dataInputGraph()[,1]
        dataGraph[,1] <- NULL
      }
      if(nrow(dataInputGraph()) > 1){
        dataGraph <- data.frame(dataInputGraph(), row.names = 1)
      }
      
      radarGraph <- rbind(max_min(), dataGraph)
      
      coul <- brewer.pal(nrow(radarGraph), "RdYlBu")
      
      radarchart(radarGraph, axistype = 1,
                 pcol=coul , pfcol=NULL , plwd=4 , plty=1,
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="white", cglwd=0.8,
                 #custom labels
                 vlcex=0.8,
      )
      legend(x=1.1, y=1, legend = rownames(radarGraph[-c(1,2),]), bty = "n", pch=20 , col=coul , text.col = "black", cex=0.7, pt.cex=3)
    }
    
    else if(input$selectgraph == "cor"){
      
      corGraph <- dataInputGraph()
      corGraph[,1] <- NULL
      
      corPlot(corGraph,
              min.length = 3,
              scale = FALSE,
              stars = TRUE,
              pval = TRUE)
    }
    
    else if(input$selectgraph == "line"){
      ggplot(dataInputGraph(), aes(x = Year, y = Data, color = Places)) +
        geom_line()+
        theme(panel.background = element_rect(fill = "#f0e5c9"))
    }
    
  })
  
  ####### Summarizing data input
  output$dataInput <- renderDataTable(placeBase)
  
}


# Creating Shiny object
shinyApp(ui = ui, server = server)