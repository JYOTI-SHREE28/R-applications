library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Scatter Plot Example in Shiny"),
  sidebarLayout(
    sidebarPanel(
      # Input controls (if necessary, like selecting a variable for plotting)
      sliderInput("numPoints", "Number of Points", min = 1, max = 100, value = 50)
    ),
    mainPanel(
      # Output area for the plot
      plotOutput("scatterPlot")
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Generate the plot based on input
  output$scatterPlot <- renderPlot({
    # Generate random data based on the slider input
    set.seed(123)
    x <- rnorm(input$numPoints)
    y <- rnorm(input$numPoints)
    
    # Plot the data
    plot(x, y, main = "Scatter Plot", xlab = "X Axis", ylab = "Y Axis", pch = 19, col = "blue")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
