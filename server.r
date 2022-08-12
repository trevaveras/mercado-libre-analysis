#server

library(shiny)
library(dplyr)
library(ggplot2)


function(input,output) {
  output$count <- renderPlot(
    data %>%
      filter(vendor==input$vendor & condition==input$condition) %>%
      group_by(brand) %>%
      count() %>%
      ggplot(aes(x=brand, y=n)) +
      geom_col(fill='lightblue') +
      ggtitle('Vendors listings')
  )
  
}