# Title: Draw ball from a box
# Description: this app simulates drawing balls from a box
# Author: Gaston Sanchez


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Drawing balls from a box"),
  
  # Sidebar with a input widgets
  sidebarLayout(
    sidebarPanel(
      numericInput("blue", "number of blue balls", 
                    min = 1, max = 20, value  = 4),
      numericInput("white", "number of white balls", 
                   min = 1, max = 20, value  = 5),
      numericInput("yellow", "number of yellow balls", 
                   min = 1, max = 20, value  = 3),
      hr(),
      numericInput("draw", "number of drawn balls",
                   min = 1, max = 10, value = 3),
      sliderInput("reps",
                  "Number of repetitions:",
                  min = 100,
                  max = 10000,
                  value = 10),
      numericInput("seed",
                   "Random Seed",
                   value = 1234)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("dist_plot"),
      tableOutput("blue_dist")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  number_blues <- reactive({
    box <- c(rep('blue', input$blue), 
             rep('white', input$white),
             rep('yellow', input$yellow))
    drawn_balls <- matrix("", nrow = input$reps, ncol = input$draw)
    set.seed(input$seed)
    for (i in 1:input$reps) {
      drawn_balls[i,] <- sample(box, input$draw)
    }
    # number of blue balls in each experiment
    apply(drawn_balls, 1, function(x) sum(x == "blue"))
  })
  
  output$dist_plot <- renderPlot({
    # draw the barchart with the distribution for number of sixes
    barplot(round(table(number_blues()) / input$reps, 2),
            las = 1, col = "gray70", border = NA,
            xlab = "Number of blue balls",
            ylab = "frequency",
            main = "Distribution of blue balls")
  })
  
  output$blue_dist <- renderTable({
    freqs <- table(number_blues()) / input$reps
    data.frame(
      balls = c('no blue', '1 blue', 'blue >= 1', 'blue >= 2'),
      probs = c(freqs[1], freqs[2], sum(freqs[2:3]), sum(freqs[3:4]))
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

