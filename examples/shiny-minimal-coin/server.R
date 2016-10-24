#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("../coin-toss-s3class.R")

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$freq_plot <- renderPlot({
    probs <- c(input$prob1, 1 - input$prob1)
    # generate bins based on input$bins from ui.R
    mycoin <- coin(c("heads", "tails"), prob = probs)
    result <- toss(mycoin, times = input$tosses)
    plot(result)
  })
  
})
