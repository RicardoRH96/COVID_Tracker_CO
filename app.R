#load R packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(DT)
library(data.table)
library(reactablefmtr)
library(highcharter)
library(pals)
library(readr)
library(lubridate)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(tmap)
library(sf)
library(rgeos)
library(leaflet.providers)
library(htmltools)
library(xts)
library(rmapshaper)
library(magrittr)


#helper function for choropleth animation

setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){
  
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))
  
  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1]
  if("label" %in% colnames(style)){
    labelData = style[,"label", FALSE]
    style = style[,-which(colnames(style)=="label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
  }
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}
#helper function in JS for choropleth animation
leafletjs <-  tags$head(
  tags$script(HTML('
  
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
  ))
)

#you only have to do this once!
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="data/world_shape_file.zip")
#system("unzip data/world_shape_file.zip")

#load spatial data
world <- st_read( 
  dsn = "data/" , 
  layer = "col_simple2"
)
world_spdf = st_as_sf(world)
world_spdf$codigo <- as.numeric(world$codigo)
world_spdf <- st_transform(world, 4326)
#world_spdf <- ms_simplify(input = world_spdf, keep = 0.3, keep_shapes = T)

#load covid data set
covidData <- read.table("data/cases_mun2.csv", sep = ",", col.names = c('Index','Week','Municipality','DIVIPOLA','Cases','Population','Incidence'))
covidData <- covidData[-1,]
covidData$Cases <- as.numeric(covidData$Cases)
covidData$Population <- as.numeric(covidData$Population)
covidData$Incidence <- as.numeric(covidData$Incidence)
#covidData <- na.omit(covidData)
#covidData <- covidData$Cases[is.na(covidData$Cases)] <- 0
covidData$Week <- as.Date(covidData$Week)

#select a certain date
selectedData <- covidData[covidData$Week == "2021-12-27",]

#match cases and spatial data via codigo/Country Code
world_spdf$Incidence <- selectedData$Incidence[match(world_spdf$codigo, selectedData$DIVIPOLA)]

#create label texts
world_spdf$LabelText <- paste0(
  "<b>Country:</b> ", world_spdf$nombre_ent,"<br>", 
  "<b>Cases per 100.000:</b> ", format(world_spdf$Incidence, nsmall=0, big.mark=","))

#define colorpalette for chart legend
paletteBins <- c(0, 10,50,100,500,1000,5000,10000,50000)
colorPalette <- colorBin(palette = "YlOrBr", domain = covidData$Incidence, bins = paletteBins, na.color = "white")

themeSelector <- function() {
  div(
    div(
      selectInput("shinytheme-selector", "Choose a theme",
                  c("default", shinythemes:::allThemes()),
                  selectize = FALSE
      )
    ),
    tags$script(
      "$('#shinytheme-selector')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'bootstrap';
        curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
        } else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
    )
  )
}

#shiny UI
ui <- navbarPage(
  title = "COVID Tracker CO",
  collapsible = TRUE,
  id = "main",
  fluid = TRUE,
tabPanel(title = "COVID-19 Case Development",
  leafletjs,
  titlePanel("COVID 19  Case Development"),
  
  sidebarPanel(width = 2,
               
               radioButtons(inputId = "mapType",
                            label = "Select Map Type",
                            choices = c("Markers", "Choropleth"),
                            selected = "Choropleth",
                            inline = TRUE),
               
               radioButtons(inputId = "frequency",
                            label = "Select Data Frequency",
                            choices = c("days", "weeks"),
                            selected = "weeks",
                            inline = TRUE
               ),
               
               uiOutput("dateUI")
               
  ),
  
  mainPanel(width = 10,
            
            leafletOutput("map", width = "95%", height = "750px")
            
  )
),
tabPanel(
  title = "Daily cases",
  plotlyOutput("casesplot"),
  sidebarPanel(
    dateRangeInput("daterange",
                   "Date Range",
                   start = "2020-03-02",
                   end = "2022-09-12",
                   format = "yyyy-mm-dd"
    ),
    helpText("Use this widget to select the time observation window"),
    a("COVID_CO Tracker was built by Ricardo Rivero H.",
      href = "https://twitter.com/Ricardo__JRH"
    ))),
tabPanel(
  title = "Daily Deaths",
  plotlyOutput("deathplot"),
  sidebarPanel(
    dateRangeInput("daterange",
                   "Date Range",
                   start = "2020-03-02",
                   end = "2022-09-12",
                   format = "yyyy-mm-dd"
    ),
    helpText("Use this widget to select the time observation window"),
    a("COVID_CO Tracker was built by Ricardo Rivero H.",
      href = "https://twitter.com/Ricardo__JRH"
    ))
),
tabPanel(
  title = "Variants circulation",
  plotlyOutput("variantplot"),
  sidebarPanel(
    dateRangeInput("daterange",
                   "Date Range",
                   start = "2020-03-02",
                   end = "2022-09-12",
                   format = "yyyy-mm-dd"
    ),
    helpText("Use this widget to select the time observation window"),
    a("COVID_CO Tracker was built by Ricardo Rivero H.",
      href = "https://twitter.com/Ricardo__JRH"
    ))
),
tabPanel(
  title = "Most Frequent Variants",
  reactableOutput("lineagetable")
)
)

themeSelector <- function() {
  div(
    div(
      selectInput("shinytheme-selector", "Choose a theme",
                  c("default", shinythemes:::allThemes()),
                  selectize = FALSE
      )
    ),
    tags$script(
      "$('#shinytheme-selector')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'bootstrap';
        curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
        } else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
    )
  )
}



#shiny server
server <- function(input, output, session) {
  
  #create slider input depending on data frequency
  observe({
    
    allDates <- unique(covidData$Week)
    eligibleDates <- allDates[xts::endpoints(allDates, on = input$frequency)]

    
    if(input$frequency == "weeks"){
      stepSize = 7
    }else{
      stepSize = 1
    }
    
    output$dateUI <- renderUI({
      sliderInput("dateSel", "Date",
                  min = min(eligibleDates),
                  max = max(eligibleDates),
                  value = min(eligibleDates),
                  step = stepSize,
                  timeFormat = "%Y-%m-%d",
                  animate = animationOptions(interval = 200, loop = FALSE)
      )
    })
  })
  
  #filter data depending on selected date
  filteredData <- reactive({
    req(input$dateSel)
    covidData[covidData$Week == input$dateSel, ]
  })
  
  #Read cases file
  cases_sum <- read.csv("data/cases_sum.csv")
  cases_sum$week <- as.Date(cases_sum$week)
  colnames(cases_sum) <- c("Week", "Cases", "Deaths")
  cases_react <- reactive({
    cases_sum %>% filter(Week >= input$daterange[1] & Week <= input$daterange[2])
  })
  
  #################################################################################################
  #read metadata for plotting lineage circulation per week
  lineage_share <- read.csv("data/strain_sum.csv")
  colnames(lineage_share) <- c('Week', 'Lineage', 'Number', 'Total','Frequency')
  lineage_share$Week <- as.Date(lineage_share$Week)
  lineage_react <- reactive({
    lineage_share %>% filter(Week >=input$daterange[1] & Week <= input$daterange[2])
  })
  
  lineage_table <- read.csv("data/strain_table.csv") 
  
  #create the base leaflet map
  output$map <- renderLeaflet({
    
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lat = 4.1156735, lng = -72.9301367, zoom = 6) %>%
      setMaxBounds(lng1 = -78.9909352282, lng2 = -66.8763258531, lat1 = -4.29818694419, lat2 = 12.4373031682) %>%
      addPolygons( 
        layerId = ~codigo,
        fillColor = "lightgray", 
        stroke = FALSE, 
        fillOpacity = 0.5,
        smoothFactor = 0.2, 
        color = "white", 
        weight = 1
      ) %>%
      
      #need to specify the leaflet::addLegend function here to avoid ambiguity with the xts::addLegend function
      leaflet::addLegend(pal = colorPalette, values = covidData$Cases, opacity = 0.9, title = "Incidence", position = "bottomleft")
    
  })
  
  
  #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
  observe({
    
    world_spdf$Incidence <- filteredData()$Incidence[match(world_spdf$codigo, filteredData()$DIVIPOLA)]
    
    world_spdf$LabelText <- paste0(
      "<b>Municipality:</b> ", world_spdf$nombre_ent,"<br>", 
      "<b>Cases per 100.000:</b> ", format(world_spdf$Incidence, nsmall=0, big.mark=","))
    
    if(input$mapType == "Markers"){
      
      leafletProxy("map", data = world_spdf) %>%
        clearMarkers() %>%
        setShapeStyle(layerId = ~codigo, fillColor = ~colorPalette(Incidence)) %>%
        addCircleMarkers(lng = ~LON,
                         lat = ~LAT,
                         radius = ~log(Incidence) * 2,
                         weight = 1,
                         opacity = 1,
                         color = ~ifelse(Incidence > 0, colorPalette(Incidence), "white"),
                         fillColor = ~colorPalette(Incidence),
                         fillOpacity = 0.8,
                         label = ~lapply(LabelText, htmltools::HTML))
      
    }else if(input$mapType == "Choropleth"){
      
      leafletProxy("map", data = world_spdf) %>%
        clearMarkers() %>%
        #setShapeStyle(layerId = ~codigo, fillColor = ~colorPalette(Cases), label = world_spdf$LabelText, fillOpacity = 0.9, smoothFactor = 0.2)
        setShapeStyle(layerId = ~codigo, fillColor = ~ifelse(Incidence > 0, colorPalette(Incidence), "white"), label = world_spdf$LabelText, fillOpacity = 0.9, smoothFactor = 0.2)
      
    }
  })
#Plot cases
  output$casesplot <- renderPlotly({
    new_cases <- ggplot(cases_react()) +
      geom_line(aes(x = Week, y = Cases))+
      labs(
        title = "New COVID-19 cases per week", 
        y = "Cases per week",
        x = "Time (week)",
        caption = "Since April 30th, 2022; Colombia's NIH has been reporting COVID-19 cases in a weekly basis"
      )+
      theme_light() +
      theme(plot.title = element_text(face = "bold", size = 22))
    
    new_cases
    
    
    ggplotly(new_cases)
  })
 #Plot deaths 
  output$deathplot <- renderPlotly({
    new_deaths <- ggplot(cases_react())+
      geom_line(aes(x = Week, y = Deaths))+
      labs(
        title = "COVID-19 Deaths per week",
        y = "Deaths per week",
        x = "Time (week)"
      )+
      theme_light()+
      theme(plot.title = element_text(face = "bold", size = 22))
    
    ggplotly(new_deaths)
  }) 
  
  
#Plot variants share  
  output$variantplot <- renderPlotly({
    lineage_plot <- ggplot(lineage_react(),aes(x=Week,y=Frequency, group=Lineage, color = Lineage)) +
      geom_smooth(method="loess", se=FALSE, fullrange=FALSE, level=0.95, inherit.aes =TRUE, position = "identity", alpha =0.7)+
      labs(
        title = "Historical Weekly Lineage Frequencies",
        y = "Lineage frequency (%)",
        x = "Time (Weeks)",
        subtitle = "Low % of sequenced samples at the start of the pandemic may account for imprecisions"
      )+
      theme_light() +
      scale_color_manual(values=as.vector(stepped(n=23)))+
      #scale_x_date(breaks = "6 month")+
      ylim(0,100)+
      theme(
        plot.title = element_text(face = "bold", size = 22) 
      ) 
    ggplotly(lineage_plot)
  })
#Plot table interactive
  output$lineagetable <- renderReactable({
    react_lineage <- reactable(lineage_table,
                               defaultColDef = colDef(
                                 cell = data_bars(lineage_table, text_position = "above",  fill_color = c("#22577A","#38A3A5","#57CC99","#80ED99","#C7F9CC"))
                               ),
                               defaultSortOrder = "desc",defaultSorted = "Total",
                               defaultPageSize = 25) %>%
      add_legend(lineage_table, col_name = 'Total', title = 'Number of Sequences', align = 'left', colors = c("#22577A","#38A3A5","#57CC99","#80ED99","#C7F9CC"))
    
  })  
  }

shinyApp(ui, server) 