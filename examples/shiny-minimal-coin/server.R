#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#source("../coin-toss-s3class.R")

library(shiny)
library(cointoss)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  result <- reactive({
    probs <- c(input$prob1, 1 - input$prob1)
    mycoin <- coin(c("heads", "tails"), prob = probs)
    set.seed(input$seed)
    toss(mycoin, times = input$tosses)
  })
  
  output$heads <- renderText({
    result()$heads
  })
  
  output$freq_plot <- renderPlot({
    plot(result())
  })
  
})
