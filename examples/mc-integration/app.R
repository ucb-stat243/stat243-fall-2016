# Title: Monte Carlo Integration
# Description: this app uses MC integration
# Author: Gaston Sanchez


library(shiny)

mc_integration <- function(fun, a, b, n) { 
  x <- runif(n, min = a, max = b)
  f <- eval(parse(text = fun))
  (b - a) * mean(f)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Monte Carlo Integration"),
  
  # Sidebar with input widgets
  sidebarLayout(
    sidebarPanel(
      textInput("func", "Write a function", 
                value = "3 + 5 * x"),
      numericInput("a", "interval's lower bound", 
                   value = 1),
      numericInput("b", "interval's upper bound", 
                   value = 2),
      sliderInput("n", "number of repetitions",
                  min = 100, max = 100000,
                  value = 1000),
      hr(),
      h5("Integral"),
      verbatimTextOutput("integral")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("dist_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$integral <- renderText({
    mc_integration(input$func, input$a, input$b, input$n)
  })
  
  output$dist_plot <- renderPlot({
    x <- seq(input$a, input$b, length.out = 100)
    y <- eval(parse(text = input$func))
    plot(x, y,
         type = "l", lwd = 3, col = "#567dc9",
         las = 1, xlim = c(input$a, input$b),
         ylim = c(min(0, min(y)), max(y)))
    abline(h = 0, col = "gray80", lwd = 2)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

