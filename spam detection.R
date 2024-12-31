library(shiny)

# Define the user interface
ui <- fluidPage(
  titlePanel("Spam Link Detector"),
  sidebarLayout(
    sidebarPanel(
      textInput("url", "Enter a URL:", ""),
      actionButton("check", "Check if Spam")
    ),
    mainPanel(
      h3("Prediction:"),
      verbatimTextOutput("result")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Load pre-trained model (simulate for this example)
  # In practice, load a real model using a library like caret or glm
  is_spam_model <- function(url) {
    # Placeholder model: classifies URLs containing "spam" as spam
    if (grepl("spam", url, ignore.case = TRUE)) {
      return("Spam")
    } else {
      return("Not Spam")
    }
  }
  
  observeEvent(input$check, {
    url <- input$url
    if (url == "") {
      output$result <- renderText("Please enter a URL.")
    } else {
      prediction <- is_spam_model(url)
      output$result <- renderText(prediction)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

