#################################################################
# Author: SANTIAGO, Gustavo N.                                  #
# Coauthors: CARCEDO, Ana; CORRENDO, Adrian; CIAMPITTI, Ignacio #
#################################################################

# Loading R packages
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(mapview)
library(scales)
library(rgdal)
library(dplyr)
library(htmltools)
library(ggplot2)
library(RColorBrewer)
library(fmsb)
library(psych)
library(shapefiles)
library(ggradar)
library(ggcorrplot)


####### Reading ShapeFile of Senegal's administrative districts
places <- readOGR("zonal_stats.shp")

####### Reading Data
placeBase <- read.csv("Test.csv", sep=";", dec=",")

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

                                         ####### Place for select to insert which data will be visualized
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
                                                          uiOutput("selectplaces"),
                                                          
                                                        ),
                                         ####### Place where users chose which year they want
                                         uiOutput("year")
                                        ),
                           ####### Main Panel
                           mainPanel(
                                     tabsetPanel(type = "tabs",
                                                  id = "tabselected",
                                                  tabPanel("Map",
                                                           value=1,
                                                           textOutput("maptxt"),
                                                           ####### Showing map
                                                           leafletOutput("mymap"),
                                                           img(id="image", src = "north-arrow.png", align = "bottom-left", width = "40px", height = "40px"),
                                                           
                                                  ),
                                                  tabPanel("Graphs",
                                                           value=2,
                                                           textOutput("plottxt"),
                                                           ####### Ploting graph
                                                           plotOutput("graph"),
                                                           downloadButton("downloadGraph", "Download Plot")
                                                  )
                                                )
                                    )
                           
                        ),
                  tabPanel("Data", 
                           ####### Rendering the table of data user uploaded
                            tags$h2("This is the raw data: "),
                            downloadButton("downloadData", "Download"),
                            dataTableOutput("dataInput")
                           ),

                  tabPanel("About", 
                           includeHTML("About.html")
                          )
                )
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
                choices = c("Bar Graph" = "Bar Graph",
                            "Radar Graph" = "Radar Graph",
                            "Correlation Graph" = "Correlation Graph",
                            "Line Graph" = "Line Graph")
    )
  })
  
  ####### Creating Dropdown for data to be ploted on graph
  output$selectfgraph <- renderUI({
    if(input$selectgraph == "Radar Graph"){
      varSelectInput("selectfgraph",
                     "Data you want to visualize on graph:",
                     subset(placeBase,select = -c(1,2)),
                     multiple = TRUE)
    }
    else if(input$selectgraph == "Correlation Graph"){
      varSelectInput("selectfgraph",
                     "Data you want to visualize on graph:",
                     subset(placeBase,select = -c(1,2)),
                     multiple = TRUE)
    }
    else if(input$selectgraph == "Bar Graph"){
      varSelectInput("selectfgraph",
                     "Data you want to visualize on graph:",
                     subset(placeBase,select = -c(1,2)))
    }
    else if(input$selectgraph == "Line Graph"){
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
    
    if(input$selectgraph == "Correlation Graph"){
      selectInput("selectplaces",
                  "Places you want to visualize on graph:",
                  choice$States)
    }
    else if (input$selectgraph != "Correlation Graph"){
      selectInput("selectplaces",
                  "Places you want to visualize on graph:",
                  choice$States,
                  multiple = TRUE)
    }
  })
  
  ####### Creating Select for period
  output$year <- renderUI({
    
    if(input$selectgraph != "Correlation Graph"){
    year <- toString(names(subset(placeBase, select = c(2))))
    choice <- placeBase %>%
      group_by(Year = eval(parse(text = year)))
    minimo <- min(choice$Year)
    maximo <- max(choice$Year)
    sliderInput("year",
                "Choose the mean between below years or one year:",
                min = minimo,
                max = maximo,
                value = c(minimo, maximo),
                step = 1,
                sep = "")
    }
    
  })
  
  ####### Filtering data for map
  dataInput <- reactive({
    place <- toString(names(subset(placeBase, select = c(1))))
    period <- toString(names(subset(placeBase, select = c(2))))
    
    placeOrder <- unique(placeBase[,1])
    
    choice <- toString(input$select)
    
    placeBase %>%
        filter(between(eval(parse(text = period)), input$year[1], input$year[2]))%>%
        group_by(Districts = eval(parse(text = place)),
                 Years = eval(parse(text = period))) %>%
        summarise(Data = mean(eval(parse(text = choice)), na.rm=TRUE)) %>%
        arrange(factor(Districts, levels = placeOrder))
    
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
    
    if (input$selectgraph == "Bar Graph"){
      
        choiceG <- toString(input$selectfgraph)
        placeBase %>%
          filter(eval(parse(text = place)) %in% input$selectplaces,
                 between(eval(parse(text = period)), input$year[1], input$year[2])) %>%
          group_by(Places = eval(parse(text = place))) %>%
          summarise(Data = mean(eval(parse(text = choiceG)), na.rm=TRUE)) 
      
    }
    
    else if (input$selectgraph == "Radar Graph"){
      
      choicesD <- as.character(input$selectfgraph)
      
      placeBase %>%
          filter(eval(parse(text = place)) %in% input$selectplaces,
                 between(eval(parse(text = period)), input$year[1], input$year[2])) %>%
          group_by(Places = eval(parse(text = place))) %>%
          summarise_at(vars(choicesD), mean, na.rm=TRUE) %>%
          ungroup() %>%
          mutate_at(vars(-Places), scales::rescale)
      
    }
    
    else if (input$selectgraph == "Correlation Graph"){
      
      choicesD <- as.character(input$selectfgraph)
      
      placeBase %>%
        filter(eval(parse(text = place)) %in% input$selectplaces) %>%
        group_by(Places = eval(parse(text = place))) %>%
        select_at(vars(choicesD))
      
    }
    
    else if(input$selectgraph == "Line Graph"){
      
      choicesD <- as.character(input$selectfgraph)
      
      placeBase %>%
        filter(eval(parse(text = place)) %in% input$selectplaces,
               between(eval(parse(text = period)), input$year[1], input$year[2])) %>%
        group_by(Year = eval(parse(text = period)),
                 Places = eval(parse(text = place))) %>%
        select(Data = choicesD)
    }
    
  })
  
  ####### Adding colors palettes
  palette <- reactive({
    colorNumeric("RdYlBu", domain = dataInput()$Data)
  })
  
  ####### Creating the information that appears when mouse-over
  labels <- reactive({
    paste("<p>", dataInput()$Districts, "<p>",
          "<p>", round(dataInput()$Data, digits = 2), "<p>",
          sep ="")
  })
  
  ####### Creating map
  map <- reactive({
    
    leaflet(width = 200, height = 300) %>%
      addTiles() %>%
      addPolygons( data = places,
                   weight = 1,
                   smoothFactor = 0.5,
                   fillOpacity = 0.8,
                   color = ~palette()(dataInput()$Data),
                   highlight = highlightOptions(
                     weight = 5,
                     color = "#666666",
                     fillOpacity = 0.7,
                     bringToFront = TRUE
                   ),
                   label = lapply(labels(), HTML)
      ) %>%
      addLegend(pal = palette(),
                values = dataInput()$Data,
                opacity = 0.7,
                position = "bottomright") %>%
      
      addEasyprint(options = easyprintOptions(position = "topright",
                                              exportOnly=TRUE,
                                              sizeModes = "CurrentSize",
                                              filename = TitleMap()))
                   
  })
  
  ####### Rendering map
  output$mymap <- renderLeaflet({
    
    map()
    
  })
  
  ####### Title of map
  TitleMap <- reactive({
    str2 = input$select
    str3 = " from "
    str4 = input$year[1]
    str5 = " to "
    str6 = input$year[2]
    str1 = " of "
    
    if (input$year[1] != input$year[2]){
      
      result = paste(str2,str3,str4,str5,str6)
      
    }
    
    else if(input$year[1] == input$year[2]){
      
      result = paste(str2,str1,str4)
      
    }
  })
  
  ###### Text to appear above the map
  output$maptxt <- renderText({
    
    TitleMap()
    
  })
  
  ####### Creating Plots
  plotInput <- reactive({
    
    if (input$selectgraph == "Bar Graph"){
      ggplot(dataInputGraph(), aes(x=Places, y=Data))+
        geom_bar(stat="identity", color="#253659", fill="#03A696")+
        geom_text(aes(label=round(Data, 2)), vjust=1.6, color="#253659", size=4)+
        theme(
          panel.background = element_rect(fill = "#c7c7c9",
                                          colour = "#f0e5c9",
                                          size = 0.5, linetype = "solid")
        )
    }
    
    else if(input$selectgraph == "Radar Graph"){
      
      dataInputGraph() %>%
          ggradar(
            font.radar = "roboto",
            grid.label.size = 0,  
            axis.label.size = 5, 
            group.point.size = 3   
          ) + 
            theme(
              legend.position = c(1, 0),  
              legend.justification = c(1, 0),
              legend.text = element_text(size = 8, family = "roboto"),
              legend.key = element_rect(fill = NA, color = NA),
              legend.background = element_blank()
            )
      
    }
    
    else if(input$selectgraph == "Correlation Graph"){
      
      corGraph <- dataInputGraph()
      corGraph[,1] <- NULL
      corr <- round(cor(corGraph), 1)
      
      ggcorrplot(corr, hc.order = TRUE, type = "lower",
                 lab = TRUE)
      
    }
    
    else if(input$selectgraph == "Line Graph"){
      ggplot(dataInputGraph(), aes(x = Year, y = Data, color = Places)) +
        geom_line()+
        theme(panel.background = element_rect(fill = "#f0e5c9"))
    }
    
  })
  
  ####### Rendering Plots
  output$graph <- renderPlot({
    
   plotInput()
    
  })
  
  ####### Title of plot
  Title <- reactive({
    
    str2 = input$selectgraph
    str7 = toString(input$selectfgraph)
    str3 = " from "
    str4 = input$year[1]
    str5 = " to "
    str6 = input$year[2]
    str1 = " of "
    
    if (input$year[1] != input$year[2]){
      
      result = paste(str2,str1,str7,str3,str4,str5,str6)
      
    }
    
    else if(input$year[1] == input$year[2]){
      
      result = paste(str2,str1,str7,str1,str4)
      
    }
  })
  
  ###### Text to appear above the graph
  output$plottxt <- renderText({
    
    Title()
    
  })
  
  ####### Downloading Plot as file
  output$downloadGraph <- downloadHandler(
    
    filename = function() {
      paste(Title(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print({plotInput()})
      dev.off()
    }
  )
  
  ####### Summarizing data input
  output$dataInput <- renderDataTable(placeBase)
  
  ####### Downloading Table as file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("SenegalDataset", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(placeBase, file, row.names = FALSE)
    }
  )
  
}


# Creating Shiny object
shinyApp(ui = ui, server = server)