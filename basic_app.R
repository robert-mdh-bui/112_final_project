
## Libraries and Options

library(shinythemes)
library(tidyverse)
library(ggthemes)
library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(fst)
options(shiny.usecairo=T)
options(scipen=999)
#options(mapbox.accessToken = 'sk.eyJ1Ijoicm9iZXJ0LW1kaC1idWkiLCJhIjoiY2szZjhtOTdnMDA2ODNvbjFsN3pna3dueiJ9.ECGieXPdhDRPNPF3jEhOGw')
#Sys.setenv('MAPBOX_TOKEN' = 'sk.eyJ1Ijoicm9iZXJ0LW1kaC1idWkiLCJhIjoiY2szZjhtOTdnMDA2ODNvbjFsN3pna3dueiJ9.ECGieXPdhDRPNPF3jEhOGw')


## Data Import

data <- read_fst("small.fst")

airports <- read_fst("airports.fst")

geoloc <- read_fst("geoloc.fst") %>% 
  group_by(AIRPORT_ID) %>% 
  summarise(
    lat = mean(LATITUDE),
    lon = mean(LONGITUDE)
  )

cols <- c("AA"="#36ace2",
          "AS"="#488509",
          "B6"="#16339f",
          "DL"="#e01e32",
          "EV"="#27446a",
          "F9"="#176642",
          "MQ"="purple",
          "NK" = "#fcec03",
          "OH"="brown",
          "OO"="steelblue",
          "UA"="#1530a2",
          "WN"="#f9a817",
          "YV"="#aaa9ad",
          "YX"="black")


## Code


ui <- fluidPage(theme = shinytheme("united"),
  titlePanel(
    h1("Air Routes and Carrier Data in the 48 Contiguous US States",align = "center")
    ),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "airlines", label = h2("Select Airlines"), 
                         choices = list("AA - American Airlines"="AA",
                                        "AS - Alaska Airlines"="AS",
                                        "B6 - JetBlue Airways"="B6",
                                        "DL - Delta Air Lines"="DL",
                                        "EV - ExpressJet Airlines"="EV",
                                        "F9 - Frontier Airlines"="F9",
                                        "MQ - Envoy Air (subsidiary of AA)"="MQ",
                                        "NK - Spirit Air Lines" = "NK",
                                        "OH - PSA Airlines (subsidiary of AA)"="OH",
                                        "OO - SkyWest Airlines"="OO",
                                        "UA - United Airlines"="UA",
                                        "WN - Southwest Airlines"="WN",
                                        "YV - Mesa Airlines"="YV",
                                        "YX - Republic Airways"="YX"),
                         selected = list("AA","DL","UA","WN", "F9","NK")),
      br(),
      br(),
      sliderInput(inputId = "cutoff", label = h3("(MAP ONLY) Set Airport Minimum N# Flights"), min = 1, max = 7198, value = 2600, step = 1),
      em("The above slider is to eliminate smaller airports and reduce noise on map, making it more visible."),
      br(),
      br(),
      submitButton(text = "Generate Map and Graphs"),
      br(),
      br(),
      em("Data Source: OTR-S Transtats, Bureau of Transportation Statistics, US Department of Transportation")
    ),
  
  mainPanel(
    plotlyOutput("outplot"),
    fluidRow(
      splitLayout(cellWidths = c("50%","50%"),
                  plotlyOutput("plot2"),
                  plotlyOutput("plot3")
                  )
    )
  )
)
)


server <- function(input, output) {
  output$plot2 <- renderPlotly({
    airlines <- input$airlines
    
    dels <- data %>% 
      filter(
        OP_UNIQUE_CARRIER %in% airlines
      ) %>% 
      group_by(OP_UNIQUE_CARRIER) %>% 
      summarise(
        delay = sum(DEP_DELAY, na.rm=T),
        Distance = sum(DISTANCE),
        flights = n_distinct(V1),
        `Average Delay` = delay / flights,
      )
    
    p1 <- dels %>% 
      ggplot()+
      geom_col(aes(x=OP_UNIQUE_CARRIER,y=Distance, fill = OP_UNIQUE_CARRIER))+
      scale_fill_manual(values=cols)+
      coord_flip()+
      labs(
        x = "Carrier",
        y = "Total Air Miles Flown by Selected Carriers (miles)",
        fill = " "
      )+
      theme(legend.position='none')
    
    ggplotly(p1, tooltip = "Distance") %>% 
      layout(
        plot_bgcolor='transparent',
        paper_bgcolor='transparent'
      )
  })
  
  output$plot3 <- renderPlotly({
    airlines <- input$airlines
    dels <- data %>% 
      filter(
        OP_UNIQUE_CARRIER %in% airlines
      ) %>% 
      group_by(OP_UNIQUE_CARRIER) %>% 
      summarise(
        delay = sum(DEP_DELAY, na.rm=T),
        distance = sum(DISTANCE),
        flights = n_distinct(V1),
        `Average Delay` = delay / flights,
      )
    
    p2 <- dels %>% 
      ggplot()+
      geom_col(aes(x=OP_UNIQUE_CARRIER,y=`Average Delay`, fill = OP_UNIQUE_CARRIER))+
      scale_fill_manual(values=cols)+
      coord_flip()+
      labs(
        x = " ",
        y = "Average Delay of Selected Carriers (minutes)",
        fill = " "
      )
    
    ggplotly(p2, tooltip = "Average Delay") %>% 
      layout(
        plot_bgcolor='transparent',
        paper_bgcolor='transparent'
      )
  })
  
  output$outplot <- renderPlotly({
    cutoff <- input$cutoff
    airlines <- input$airlines

    
    lines <- data %>% 
      filter(
        OP_UNIQUE_CARRIER %in% airlines
      ) %>% 
      group_by(OP_UNIQUE_CARRIER,ORIGIN_AIRPORT_ID,ORIGIN_CITY_NAME,DEST_AIRPORT_ID) %>% 
      summarise(
        n = n_distinct(V1)
      ) %>% 
      filter(
        n >= cutoff
      ) %>% 
      distinct()
    
    lines <- left_join(lines,geoloc,by = c("ORIGIN_AIRPORT_ID"="AIRPORT_ID")) %>% 
      mutate(
        ori_lat = lat,
        ori_lon = lon
      ) %>% 
      select(-c(lat,lon))
    
    lines <- left_join(lines,geoloc,by = c("DEST_AIRPORT_ID"="AIRPORT_ID")) %>% 
      mutate(
        des_lat = lat,
        des_lon = lon
      ) %>% 
      select(-c(lat,lon))
    
    lines2 <- lines %>% 
      group_by(OP_UNIQUE_CARRIER,ORIGIN_AIRPORT_ID,ORIGIN_CITY_NAME,ori_lon,ori_lat) %>% 
      summarise(
        m = sum(n)
      ) 
    
    lmax <- lines2 %>% 
      ungroup() %>% 
      group_by(ORIGIN_AIRPORT_ID) %>% 
      summarise(
        max = max(m)
      )
    
    lmarker <- left_join(lmax,lines2,by=c("max"="m","ORIGIN_AIRPORT_ID"="ORIGIN_AIRPORT_ID")) %>% 
      distinct()
    
    lmarker <- left_join(lmarker, airports, by=c("ORIGIN_AIRPORT_ID"="ORIGIN_AIRPORT_ID"))
    
    geo <- list(
      scope = "usa",
      #projection = list(
       #type = 'orthographic',
       # rotation = list(lon = -100, lat = 40, roll = 0)
      #),
      showland = T,
      landcolor = 'transparent',
      countrycolor = 'transparent'
    )
    
    p <- plot_geo() %>%
      add_segments(
        data = lines, x = ~ori_lon, xend = ~des_lon,
        y = ~ori_lat, yend = ~des_lat,
        hoverinfo = "none",
        color = ~OP_UNIQUE_CARRIER,
        colors = cols,
        alpha=.175, size=I(1)
      ) %>% 
      add_markers(
        data = lmarker, x = ~ori_lon, y = ~ori_lat, 
        text = ~paste("Airport:", ORIGIN, "|",name, "<br>City: ", ORIGIN_CITY_NAME,"<br>Busiest Airline in Selection: ", OP_UNIQUE_CARRIER), 
        hoverinfo = "text",
        size = ~max^2, 
        alpha=1,
        color = ~OP_UNIQUE_CARRIER,
        colors = cols
      )
    
    
    ggplotly(p) %>% 
      layout(
        title = '2018 Map of Airports and Air Routes in the 48 States',
        geo = geo, showlegend = TRUE,
        plot_bgcolor='transparent',
        paper_bgcolor='transparent'
      )
  })
}
shinyApp(ui = ui, server = server)