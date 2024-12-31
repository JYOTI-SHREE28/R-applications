# Load necessary libraries
library(shiny)
library(lubridate)
library(ggplot2)
library(shinyjs)

# UI
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs for interactivity
  titlePanel("Study Manager"),
  sidebarLayout(
    sidebarPanel(
      textInput("subject", "Subject:", placeholder = "e.g., Math"),
      dateInput("date", "Study Date:", value = Sys.Date()),
      numericInput("duration", "Study Duration (minutes):", value = 60, min = 1, max = 480),
      actionButton("addSession", "Add Study Session"),
      br(),
      br(),
      textOutput("status")
    ),
    mainPanel(
      h4("Upcoming Study Sessions"),
      tableOutput("sessionList"),
      h4("Study Progress"),
      plotOutput("progressPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  studySessions <- reactiveVal(data.frame(Subject = character(), Date = as.Date(character()), Duration = numeric(), Completed = logical(), stringsAsFactors = FALSE))
  
  observeEvent(input$addSession, {
    subject <- input$subject
    date <- input$date
    duration <- input$duration
    
    if (subject == "") {
      showNotification("Please enter a subject name.", type = "error")
      return()
    }
    
    # Add new study session
    newSession <- data.frame(Subject = subject, Date = date, Duration = duration, Completed = FALSE, stringsAsFactors = FALSE)
    currentSessions <- studySessions()
    updatedSessions <- rbind(currentSessions, newSession)
    studySessions(updatedSessions)
    
    # Display success message
    output$status <- renderText({
      paste("Study session for", subject, "on", date, "set for", duration, "minutes.")
    })
    
    # Clear input fields
    updateTextInput(session, "subject", value = "")
    updateDateInput(session, "date", value = Sys.Date())
    updateNumericInput(session, "duration", value = 60)
  })
  
  # Display upcoming sessions
  output$sessionList <- renderTable({
    upcomingSessions <- studySessions()[studySessions()$Date >= Sys.Date(), ]
    upcomingSessions
  })
  
  # Progress tracking (log completed study time)
  observeEvent(input$addSession, {
    currentSessions <- studySessions()
    completedSessions <- currentSessions[currentSessions$Date <= Sys.Date(), ]
    completedSessions$Completed <- TRUE
    studySessions(completedSessions)
  })
  
  # Plot study progress
  output$progressPlot <- renderPlot({
    sessions <- studySessions()
    if (nrow(sessions) > 0) {
      progressData <- aggregate(Duration ~ Subject, data = sessions[sessions$Completed == TRUE, ], sum)
      
      ggplot(progressData, aes(x = Subject, y = Duration, fill = Subject)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Study Progress", x = "Subject", y = "Total Study Time (minutes)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
