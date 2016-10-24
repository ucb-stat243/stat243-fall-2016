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
    # tossing a coin
    set.seed(input$seed)
    mycoin <- coin(c("heads", "tails"), prob = probs)
    result <- toss(mycoin, times = input$tosses)
  })
  
  output$freq_plot <- renderPlot({
    # plot of relative frequencies
    plot(result())
  })
  
  output$frequencies <- renderTable({
    data.frame(
      count = c(result()$heads, result()$tails),
      prop = c(result()$heads, result()$tails) / input$tosses
    )
  })
  
})
