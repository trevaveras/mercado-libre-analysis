#UI

library(shiny)

fluidPage(
  titlePanel("Smartphone Marketplace Analytics 
     Insights from MercadoLibre e-commerce data in Peru"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId='vendor',
                     label='vendor',
                     choices=unique(data$vendor)),
      selectizeInput(inputId='condition',
                     label='condition',
                     choices=unique(data$condition))
      
      
    ),
    mainPanel(
      plotOutput('count'))
     
    )
  )


