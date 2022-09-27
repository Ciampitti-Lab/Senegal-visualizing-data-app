#################################################################
# Author: SANTIAGO, Gustavo N.                                  #
# Coauthors: CARCEDO, Ana; CIAMPITTI, Ignacio; BROWN, Molly E.  #
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
library(plotly)
library(reshape2)

places <- readOGR("Data/zonal_stats.shp")

placeBase <- read.csv("Data/Full Data.csv", sep=",", dec=".")
units <- read.csv("Data/Units.csv", sep = ",", dec = ".")

ui <- fluidPage(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }",
                           ".selectize-input {height: 10px; width: 300px; font-size: 10px; margin: 3px}"),
                navbarPage(
                  theme = "united",
                  "Senegal Data Visualization",
                  tabPanel("Instructions"),
                  tabPanel("Visualization",
                           sidebarPanel(conditionalPanel(condition = "input.tabselected==1",
                                                          uiOutput("select"),
                                                          uiOutput("yearmap")
                                                          ),
                                         conditionalPanel(style ="overflow-y:scroll; max-height: 65vh; padding: 5px",
                                                          condition = "input.tabselected==2",
                                                          uiOutput("placeoryearSIAF"),
                                                          uiOutput("selectplacesSIAF"),
                                                          uiOutput("yearSIAF"),
                                                          h6("Select what data you want to visualize on radar plot:"),
                                                          uiOutput("productivity"),
                                                          uiOutput("economic"),
                                                          #uiOutput("environmental"),
                                                          uiOutput("human"),
                                                          uiOutput("social")
                                                          ),
                                         conditionalPanel(condition = "input.tabselected==3",
                                                          uiOutput("placesBar"),
                                                          uiOutput("yearBar"),
                                                          uiOutput("dataBar")
                                                        ),
                                         conditionalPanel(condition = "input.tabselected==4",
                                                          uiOutput("placeCor"),
                                                          uiOutput("yearCor"),
                                                          uiOutput("dataCor")
                                                        ),
                                         conditionalPanel(condition = "input.tabselected==5",
                                                          uiOutput("placesLine"),
                                                          uiOutput("yearLine"),
                                                          uiOutput("dataLine")
                                                        )
                                         ),
                           mainPanel(
                                     tabsetPanel(type = "tabs",
                                                  id = "tabselected",
                                                  tabPanel("Map",
                                                           value=1,
                                                           textOutput("maptxt"),
                                                           leafletOutput("mymap"),
                                                           img(id="image", src = "north-arrow.png", align = "bottom-left", width = "40px", height = "40px")
                                                  ),
                                                 tabPanel("SIAF",
                                                          value=2,
                                                          textOutput("plottxtSIAF"),
                                                          plotlyOutput("graphSIAF")
                                                  ),
                                                  tabPanel("Bar Chart",
                                                           value=3,
                                                           textOutput("plottxtBar"),
                                                           plotlyOutput("graphBar")
                                                  ),
                                                 tabPanel("Correlation Chart",
                                                          value=4,
                                                          textOutput("plottxtCor"),
                                                          plotlyOutput("graphCor")
                                                 ),
                                                 tabPanel("Line Chart",
                                                          value=5,
                                                          textOutput("plottxtLine"),
                                                          plotlyOutput("graphLine")
                                                 )
                                                )
                                        )
                           ),
                  tabPanel("Data",
                            tags$h2("This is the raw data: "),
                            downloadButton("downloadData", "Download"),
                            dataTableOutput("dataInput")
                           ),
                  tabPanel("About", 
                           includeHTML("About.html")
                          )
                  )
              )


server <- function(input, output, session) {
  
  ####### Map
  
  output$select <- renderUI({
    varSelectInput("select",
                   "Data you want to visualize on map:",
                   subset(placeBase,select = -c(1,2)))
  })
  
  output$yearmap <- renderUI({
    
    year <- toString(names(subset(placeBase, select = c(2))))
    choice <- placeBase %>%
      group_by(Year = eval(parse(text = year)))
    minimo <- min(choice$Year)
    maximo <- max(choice$Year)
    sliderInput("yearmap",
                "Choose the mean between below years or one year:",
                min = minimo,
                max = maximo,
                value = c(minimo, maximo),
                step = 1,
                sep = "")
    
  })
  
  dataInput <- reactive({
    place <- toString(names(subset(placeBase, select = c(1))))
    period <- toString(names(subset(placeBase, select = c(2))))
    
    placeOrder <- unique(placeBase[,1])
    
    choice <- toString(input$select)
    
    placeBase %>%
      filter(between(eval(parse(text = period)), input$yearmap[1], input$yearmap[2]))%>%
      group_by(Districts = eval(parse(text = place))) %>%
      summarise(Data = mean(eval(parse(text = choice)), na.rm=TRUE)) %>%
      arrange(factor(Districts, levels = placeOrder))
    
  })

  palette <- reactive({
    colorNumeric("RdYlBu", domain = dataInput()$Data)
  })

  labels <- reactive({
    titulo <- toString(input$select)
    unity <- units %>%
      filter(Name == titulo)
    unidade <- toString(unity$Unit)
    
    paste("<p>", dataInput()$Districts, "<p>",
          "<p>", titulo, ": ", round(dataInput()$Data, digits = 2), " ", unidade, "<p>",
          sep ="")
  })

  output$mymap <- renderLeaflet({
    
    titulo <- toString(input$select)
    unity <- units %>%
      filter(Name == titulo)
    unidade <- toString(unity$Unit)
    
    
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
                    position = "bottomright",
                    title = unidade)
    
    
  })
  
  output$maptxt <- renderText({
    
    str2 = input$select
    str3 = " from "
    str4 = input$yearmap[1]
    str5 = " to "
    str6 = input$yearmap[2]
    str1 = " of "
    
    if (input$yearmap[1] != input$yearmap[2]){
      
      result = paste(str2,str3,str4,str5,str6)
      
    }
    
    else if(input$yearmap[1] == input$yearmap[2]){
      
      result = paste(str2,str1,str4)
      
    }
    
    result
    
  })
  
  ####### SIAF
  
  output$placeoryearSIAF <- renderUI({
    
    selectInput("placeoryearSIAF",
                 h6("Compare different districts in years or the same district in different years"),
                 choices = c("Compare Districts" = "MD",
                             "Compare Years" = "SD")
    )
    
  })
  
  output$selectplacesSIAF <- renderUI({
    place <- toString(names(subset(placeBase, select = c(1))))
    choice <- placeBase %>%
      group_by(States = eval(parse(text = place)))
    
    if(input$placeoryearSIAF == "MD"){
      
      selectizeInput("selectplacesSIAF",
                     h6("Places you want to visualize on graph:"),
                     choice$States,
                     multiple = TRUE,
                     options = list(maxItems = 10))
    }
    
    else if(input$placeoryearSIAF == "SD"){
      selectInput("selectplacesSIAF",
                  h6("Place you want to visualize on graph:"),
                  choice$States)
    }
    
  })
  
  output$yearSIAF <- renderUI({
    
    year <- toString(names(subset(placeBase, select = c(2))))
    choice <- placeBase %>%
      group_by(Year = eval(parse(text = year)))
    minimo <- min(choice$Year)
    maximo <- max(choice$Year)
    
    if(input$placeoryearSIAF == "MD"){
      sliderInput("yearSIAF",
                  h6("Choose the mean between below years or one year:"),
                  min = minimo,
                  max = maximo,
                  value = c(minimo, maximo),
                  step = 1,
                  sep = "")
    }
    
    else if(input$placeoryearSIAF == "SD"){
      
      selectizeInput("yearSIAF",
                     h6("Choose years you want to compare"),
                     choice$Year,
                     multiple = TRUE,
                     options = list(maxItems = 10)
      )
      
    }
    
  })
  
  output$productivity <- renderUI({
    
    varSelectizeInput("productivity",
                      h6("Productivity"),
                      subset(placeBase,select = c(3:14)),
                      multiple = TRUE,
                      options = list(maxItems = 2))
    
  })
  
  output$economic <- renderUI({
    
    varSelectizeInput("economic",
                      h6("Economic"),
                      subset(placeBase,select = c(15:33)),
                      multiple = TRUE,
                      options = list(maxItems = 2))
    
  })
  
  # output$environmental <- renderUI({
  #   
  #   varSelectizeInput("environmental",
  #                     h6("Environmental"),
  #                     subset(placeBase,select = -c(1,2)),
  #                     multiple = TRUE,
  #                     options = list(maxItems = 2))
  #   
  # })
  
  output$human <- renderUI({
    
    varSelectizeInput("human",
                      h6("Human"),
                      subset(placeBase,select = c(34:49)),
                      multiple = TRUE,
                      options = list(maxItems = 2))
    
  })
  
  output$social <- renderUI({
    
    varSelectizeInput("social",
                      h6("Social"),
                      subset(placeBase,select = c(50:62)),
                      multiple = TRUE,
                      options = list(maxItems = 2))
    
  })
  
  dataInputGraphSIAF <- reactive({
    place <- toString(names(subset(placeBase, select = c(1))))
    period <- toString(names(subset(placeBase, select = c(2))))
    
    productivity <- as.character(input$productivity)
    economic <- as.character(input$economic)
    #environmental <- as.character(input$environmental)
    human <- as.character(input$human)
    social <- as.character(input$social)
    choices <- c(productivity, economic, human, social)
    
    if(input$placeoryearSIAF == "MD"){
      
      placeBase %>%
        filter(eval(parse(text = place)) %in% input$selectplacesSIAF,
               between(eval(parse(text = period)), input$yearSIAF[1], input$yearSIAF[2])) %>%
        group_by(Places = eval(parse(text = place))) %>%
        summarise_at(vars(choices), mean, na.rm=TRUE) %>%
        ungroup() %>%
        mutate_at(vars(-Places), scales::rescale) %>%
        mutate_at(vars(-Places), ~coalesce(.,0))
      
    }
    
    else if(input$placeoryearSIAF == "SD"){
      
      placeBase %>%
        filter(eval(parse(text = place)) == input$selectplacesSIAF,
               eval(parse(text = period)) %in% input$yearSIAF) %>%
        group_by(Years = eval(parse(text = period))) %>%
        summarise_at(vars(choices), mean, na.rm=TRUE) %>%
        ungroup() %>%
        mutate_at(vars(-Years), scales::rescale)%>%
        mutate_at(vars(-Years), ~coalesce(.,0))
      
    }
    
  })
  
  output$graphSIAF <- renderPlotly({
    
    graph <- dataInputGraphSIAF() %>%
        ggradar(
          font.radar = "roboto",
          grid.label.size = 2.5,
          axis.label.size = 3,
          group.point.size = 3
        ) + 
        theme(
          legend.position = "top",
          legend.justification = c(1, 0),
          legend.text = element_text(size = 8, family = "roboto"),
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_blank(),
          axis.text = element_text(size=7, angle=90, vjust=5, hjust=5)
        )
    
    ggplotly(graph)
    
  })
  
  output$plottxtSIAF <- renderText({
    
    str2 = "Radar Plot "
    str7 = toString(input$selectfgraphSIAF)
    str3 = "from "
    str4 = input$yearSIAF[1]
    str5 = " to "
    str6 = input$yearSIAF[2]
    str1 = "of "
    str8 = input$selectplacesSIAF
    
    if(input$placeoryearSIAF == "MD"){
      
      if (input$yearSIAF[1] != input$yearSIAF[2]){
        
        result = paste(str2, str3, str4, str5, str6)
        
      }
      
      else if(input$yearSIAF[1] == input$yearSIAF[2]){
        
        result = paste(str2, str1, str4)
        
      }
      
    }
    
    else if(input$placeoryearSIAF == "SD"){
      
      result = paste(str2,str3,str8)
      
    }
    
    result
    
  })
  
  ####### Bar Plot
  
  output$placesBar <- renderUI({
    place <- toString(names(subset(placeBase, select = c(1))))
    choice <- placeBase %>%
      group_by(States = eval(parse(text = place)))
    
    selectizeInput("placesBar",
                   h6("Places you want to visualize on graph:"),
                   choice$States,
                   multiple = TRUE,
                   options = list(maxItems = 10))
    
  })
  
  output$yearBar <- renderUI({
    
    year <- toString(names(subset(placeBase, select = c(2))))
    choice <- placeBase %>%
      group_by(Year = eval(parse(text = year)))
    minimo <- min(choice$Year)
    maximo <- max(choice$Year)
    
    sliderInput("yearBar",
                h6("Choose the mean between below years or one year:"),
                min = minimo,
                max = maximo,
                value = c(minimo, maximo),
                step = 1,
                sep = "")
    
  })
  
  output$dataBar <- renderUI({
    
    varSelectizeInput("dataBar",
                      h6("Select what data you want to visualize: "),
                      subset(placeBase,select = -c(1,2))
                      )
    
  })
  
  dataInputBar <- reactive({
    place <- toString(names(subset(placeBase, select = c(1))))
    period <- toString(names(subset(placeBase, select = c(2))))
    choiceG <- toString(input$dataBar)
    
    placeBase %>%
      filter(eval(parse(text = place)) %in% input$placesBar,
             between(eval(parse(text = period)), input$yearBar[1], input$yearBar[2])) %>%
      group_by(Places = eval(parse(text = place))) %>%
      summarise(Data = mean(eval(parse(text = choiceG)), na.rm=TRUE))
    
  })
  
  output$graphBar <- renderPlotly({
    
    choiceD <- toString(input$dataBar)
    
    barPlot <- ggplot(dataInputBar(), aes(x=Places, y=Data))+
                  geom_bar(stat="identity", color="#253659", fill="#03A696")+
                  geom_text(aes(label=round(Data, 2)), vjust=1.6, color="#253659", size=4)+
                  theme(
                    panel.background = element_rect(fill = "#c7c7c9",
                                                    colour = "#c7c7c9",
                                                    size = 0.5, linetype = "solid")
                  )+
                  labs(y = choiceD)
    
    ggplotly(barPlot)
    
  })

  output$plottxtBar <- renderText({
    
    str2 = "Bar Chart"
    str7 = toString(input$dataBar)
    str3 = " from "
    str4 = input$yearBar[1]
    str5 = " to "
    str6 = input$yearBar[2]
    str1 = " of "
    
    if (input$yearBar[1] != input$yearBar[2]){
      
      result = paste(str2,str1,str7,str3,str4,str5,str6)
      
    }
    
    else if(input$yearBar[1] == input$yearBar[2]){
      
      result = paste(str2,str1,str7,str1,str4)
      
    }
    
    result
  })
  
  ####### Correlation Plot
  
  output$placeCor <- renderUI({
    
    place <- toString(names(subset(placeBase, select = c(1))))
    choice <- placeBase %>%
      group_by(States = eval(parse(text = place)))
    
    selectInput("placeCor",
                h6("Places you want to visualize on graph:"),
                choice$States)
    
  })
  
  output$yearCor <- renderUI({
    
    year <- toString(names(subset(placeBase, select = c(2))))
    choice <- placeBase %>%
      group_by(Year = eval(parse(text = year)))
    minimo <- min(choice$Year)
    maximo <- max(choice$Year)
    
    sliderInput("yearCor",
                h6("Choose years below which you want to correlate"),
                min = minimo,
                max = maximo,
                value = c(minimo, maximo),
                step = 1,
                sep = "")
    
  })
  
  filterCor <- reactive({
    
    place <- toString(names(subset(placeBase, select = c(1))))
    period <- toString(names(subset(placeBase, select = c(2))))
    
    placeBase %>%
      filter(eval(parse(text = place)) %in% input$placeCor,
             between(eval(parse(text = period)), input$yearCor[1], input$yearCor[2])) %>%
      select_if(~ !any(is.na(.)))
    
  })
  
  output$dataCor <- renderUI({
    
    varSelectizeInput("dataCor",
                      h6("Select what data you want to correlate: "),
                      subset(filterCor(),select = -c(1,2)),
                      multiple = TRUE,
                      options = list(maxItems = 10)
          )
    
  })
  
  dataInputCor <- reactive({
    
    place <- toString(names(subset(filterCor(), select = c(1))))
    period <- toString(names(subset(filterCor(), select = c(2))))
    choicesD <- as.character(input$dataCor)
    
    filterCor() %>%
      group_by(Places = eval(parse(text = place)))%>%
      select_at(vars(choicesD))
    
  })
  
  output$graphCor <- renderPlotly({
    
    corGraph <- dataInputCor()
    corGraph[,1] <- NULL
    corr <- round(cor(corGraph), 1)
    
    corPlot <- ggcorrplot(corr, hc.order = TRUE, type = "lower",lab = TRUE, lab_size = 2) +
      theme(axis.text.x=element_text(size=7, angle=45, vjust=1, hjust=1, 
                                     margin=margin(-3,0,0,0)),
            axis.text.y=element_text(size=7, margin=margin(-3,0,0,0)),
            panel.grid.major=element_blank()) 
    
    ggplotly(corPlot)
    
  })
  
  output$plottxtCor <- renderText({
    
    str2 = "Correlation Matrix"
    str1 = " of "
    str7 = toString(input$dataCor)
    str3 = " from "
    str4 = toString(input$yearCor[1])
    str5 = " to "
    str6 = toString(input$yearCor[2])
    
    result = paste(str2,str1,str7,str3,str4,str5,str6)
    
  })
  
  ###### Line Plot
  
  output$placesLine <- renderUI({
    
    place <- toString(names(subset(placeBase, select = c(1))))
    choice <- placeBase %>%
      group_by(States = eval(parse(text = place)))
    
    selectizeInput("placesLine",
                   h6("Places you want to visualize on graph:"),
                   choice$States,
                   multiple = TRUE,
                   options = list(maxItems = 10))
    
  })
  
  output$yearLine <- renderUI({
    
    year <- toString(names(subset(placeBase, select = c(2))))
    choice <- placeBase %>%
      group_by(Year = eval(parse(text = year)))
    minimo <- min(choice$Year)
    maximo <- max(choice$Year)
    
    sliderInput("yearLine",
                h6("Choose the mean between below years or one year:"),
                min = minimo,
                max = maximo,
                value = c(minimo, maximo),
                step = 1,
                sep = "")
    
  })
  
  output$dataLine <- renderUI({
    
    varSelectInput("dataLine",
                       h6("Select what data you want to correlate: "),
                       subset(placeBase,select = -c(1,2)))
    
  })
  
  dataInputLine <- reactive({
    
    place <- toString(names(subset(placeBase, select = c(1))))
    period <- toString(names(subset(placeBase, select = c(2))))
    choicesD <- as.character(input$dataLine)
    
    placeBase %>%
      filter(eval(parse(text = place)) %in% input$placesLine,
             between(eval(parse(text = period)), input$yearLine[1], input$yearLine[2])) %>%
      group_by(Year = eval(parse(text = period)),
               Places = eval(parse(text = place))) %>%
      select(Data = choicesD)
    
    
    
  })
  
  output$graphLine <- renderPlotly({
    
    choiceD <- toString(input$dataLine)
    
    plotLine <- ggplot(dataInputLine(), aes(x = Year, y = Data, color = Places)) +
                        geom_line()+
                        theme(panel.background = element_rect(fill = "#c7c7c9"))+
                        labs(y = choiceD)
    
    ggplotly(plotLine)
    
  })
  
  output$plottxtLine <- renderText({
    
    str2 = "Line Chart"
    str7 = toString(input$dataLine)
    str3 = " from "
    str4 = input$yearLine[1]
    str5 = " to "
    str6 = input$yearLine[2]
    str1 = " of "
    
    if (input$yearLine[1] != input$yearLine[2]){
      
      result = paste(str2,str1,str7,str3,str4,str5,str6)
      
    }
    
    else if(input$yearLine[1] == input$yearLine[2]){
      
      result = paste(str2,str1,str7,str1,str4)
      
    }
    
    result
    
  })
  
  output$dataInput <- renderDataTable({placeBase},options = list(scrollX = TRUE))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("SenegalDataset", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(placeBase, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)