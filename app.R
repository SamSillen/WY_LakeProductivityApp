#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(shiny)
library(leaflet)
library(scales)


trophic_data <- read.csv("data/WY_ProductivityApp_TrophicState_Data.csv")
color_data <- read.csv("data/WY_ProductivityApp_ColorData.csv")

color_data$date <- as_date(color_data$date)

trophic_data$lagoslakeid <- as.character(trophic_data$lagoslakeid)

pal <- colorFactor(c('#698c86', '#7dae38', '#2158bc' ), domain = c("no trend","trending oligotrophic", "trending eutrophic" ))

trophic_data$trend <- factor(trophic_data$trend, levels = c('no trend', 'trending oligotrophic', 'trending eutrophic'))

fui.colors <- c(
  "#2158bc", "#316dc5", "#327cbb", "#4b80a0", "#568f96", "#6d9298", "#698c86", 
  "#759e72", "#7ba654", "#7dae38", "#94b660","#94b660", "#a5bc76", "#aab86d", 
  "#adb55f", "#a8a965", "#ae9f5c", "#b3a053", "#af8a44", "#a46905", "#9f4d04")


min.fui <- min(color_data$fui, na.rm = TRUE)
max.fui <- max(color_data$fui, na.rm = TRUE)



ui <- fluidPage(titlePanel("Landsat Observations of Lake Water Quality in Wyoming"),
                column(7, leafletOutput("wsmap", height = "1000")),
                column(5, plotOutput("plot", height = "500")),
                column(5, plotOutput("plot2", height = "500")))

server <- function(input, output) {
  
  output$wsmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = trophic_data,lat = trophic_data$lake_lat_decdeg, lng = trophic_data$lake_lon_decdeg, weight = 2, fillColor = ~pal(trend), color = 'black', stroke = FALSE, fillOpacity = .8, radius = 5, layerId= trophic_data$lagoslakeid, popup = trophic_data$lake_namelagos) 
  })
  
  # generate plots
  ggplot_data <- reactive({
    lagoslakeid <- input$wsmap_marker_click$id
    trophic_data[trophic_data$lagoslakeid %in% lagoslakeid,]
  })
  
  #generate plots
  output$plot <- renderPlot({
    ggplot(data = ggplot_data(), aes(x= year, y = value, fill = Trophic_State)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c( '#7dae38',  '#698c86', '#2158bc')) +
      xlab("Year") +
      ylab("Percent") +
      ggtitle(label = "Predicted Trophic State") +
      theme_bw() +
      theme(legend.title = element_blank(), axis.title = element_text(size = 30), axis.text = element_text(size = 24, color = 'black'), legend.text = element_text(size = 30), title = element_text(size = 30, face = 'bold'), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'bottom')
  })
  # generate plots
  ggplot_data2 <- reactive({
    lagoslakeid <- input$wsmap_marker_click$id
    color_data[color_data$lagoslakeid %in% lagoslakeid,]
  })
  
  #generate plots
  output$plot2 <- renderPlot({

    ggplot(data = ggplot_data2(), aes(x= date, y = dWL)) +
      scale_x_date(breaks = date_breaks('10 years'),  labels=date_format('%Y')) +
      geom_point(aes(color = fui), size = 3) +
      geom_smooth(se = T, method = 'lm') +
      scale_color_gradientn(colours = fui.colors[min.fui:max.fui]) +
      labs(y = 'Wavelenght (nm)', x = 'Year', title = 'Observed Lake Color') +
      theme_bw() +
      theme(legend.title = element_blank(), axis.title = element_text(size = 30), axis.text = element_text(size = 24, color = 'black'), legend.text = element_text(size = 30), title = element_text(size = 30, face = 'bold'), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'null')  
  })
}

shinyApp(ui, server)

