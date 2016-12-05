
library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Uniformly random numbers"),
  
  # Sidebar with a input widgets
  sidebarLayout(
    sidebarPanel(
      sliderInput("size", "Sample size", 
                  min = 10, max = 10000, value = 500)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("dist_plot")
    )
  )
)

# Define server logic required to draw plot
server <- function(input, output) {
  
  output$dist_plot <- renderPlot({
    # hist(runif(input$size, min = -1, max = 1))
    set.seed(12345)
    x <- 1:input$size
    y <- runif(input$size, min = -1, max = 1)
    plot(x, y, col = "#77777788", pch = 20)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

