library(shiny)
library(tidyverse)
source("UCB.R")
ui <- fluidPage(
  sliderInput("prob","Choose probability",min=0.05,max=0.95, step=0.05,value=0.55),
  plotOutput("hist"),
  verbatimTextOutput("ch")
)
server <- function(input, output, session) {

output$hist <-   renderPlot(

  # N=1000

    ggplot(UCB(1000,probs=c(0.5,input$prob)),aes(x=chosen)) + geom_histogram(binwidth = 0.5,fill="firebrick") 
    
  )
output$ch <- renderPrint(input$prob)
}
shinyApp(ui, server)
