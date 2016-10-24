#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that graphs relative frequencies
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Coin Tossing"),
  
  # Sidebar with a slider input for number of tosses 
  sidebarLayout(
    sidebarPanel(
       sliderInput(inputId = "tosses",
                   label = "Number of tosses:",
                   min = 1,
                   max = 10000,
                   value = 1000),
       sliderInput(inputId = "prob1", 
                   label = "proportion of heads", 
                   min = 0,
                   max = 1,
                   value = 0.5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("freq_plot")
    )
  )
))
