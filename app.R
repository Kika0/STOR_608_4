library(shiny)
library(tidyverse)
source("UCB.R")
source("TS.R")
ui <- fluidPage(
  fluidRow(
column(3,
  sliderInput("prob","Choose probability",min=0.05,max=0.95, step=0.05,value=0.5)),
column(3,
  sliderInput("prob1","Choose probability",min=0.05,max=0.95, step=0.05,value=0.55)),
column(3,
  sliderInput("a","Choose alpha",min=0.5,max=5, step=0.5,value=1)),
column(3,
  sliderInput("b","Choose beta",min=0.5,max=5, step=0.5,value=1))),
  hr(),
  fluidRow(
    
  column(6,
         h2("UCB1"),
         plotOutput("hist")
    ),
  column(6,
h2("Thompson sampling"),
  plotOutput("hist1")
  )
  
  )
  # verbatimTextOutput("ch")
)
server <- function(input, output, session) {

output$hist <-   renderPlot(

  # N=1000

    ggplot(UCB(1000,probs=c(input$prob,input$prob1)),aes(x=chosen)) + geom_histogram(binwidth = 0.5,fill="firebrick") 
    
  )
output$ch <- renderPrint(input$prob)

output$hist1 <-   renderPlot(
  
  # N=1000
  
  ggplot(TS(1000,probs=c(input$prob,input$prob1),alpha=input$a,beta=input$b),aes(x=chosen)) + geom_histogram(binwidth = 0.5,fill="firebrick") 
  
)

}
shinyApp(ui, server)
