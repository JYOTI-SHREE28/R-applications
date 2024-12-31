library(shiny)

# Define the deck of cards
suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
values <- c(2:10, "Jack", "Queen", "King", "Ace")
deck <- expand.grid(Value = values, Suit = suits)
deck$Rank <- rep(2:14, 4) # Assign numeric ranks to card values
deck$Card <- paste(deck$Value, "of", deck$Suit)

# Shuffle the deck
shuffle_deck <- function() {
  deck[sample(nrow(deck)), ]
}

# UI
ui <- fluidPage(
  titlePanel("Higher or Lower Card Game"),
  sidebarLayout(
    sidebarPanel(
      actionButton("start", "Start Game"),
      actionButton("higher", "Higher"),
      actionButton("lower", "Lower"),
      textOutput("result")
    ),
    mainPanel(
      h3("Current Card"),
      textOutput("current_card"),
      h3("Next Card"),
      textOutput("next_card")
    )
  )
)

# Server
server <- function(input, output, session) {
  game_deck <- reactiveVal()
  current_index <- reactiveVal(1)
  current_card <- reactiveVal()
  next_card <- reactiveVal()
  result <- reactiveVal("")
  
  observeEvent(input$start, {
    game_deck(shuffle_deck())
    current_index(1)
    current_card(game_deck()$Card[1])
    next_card("")
    result("")
  })
  
  output$current_card <- renderText({
    current_card()
  })
  
  output$next_card <- renderText({
    next_card()
  })
  
  output$result <- renderText({
    result()
  })
  
  observeEvent(input$higher, {
    if (current_index() < nrow(game_deck())) {
      next_index <- current_index() + 1
      next_card(game_deck()$Card[next_index])
      if (game_deck()$Rank[next_index] > game_deck()$Rank[current_index()]) {
        result("Correct! The next card is higher.")
      } else {
        result("Wrong! The next card is not higher.")
      }
      current_index(next_index)
      current_card(game_deck()$Card[next_index])
    } else {
      result("No more cards in the deck!")
    }
  })
  
  observeEvent(input$lower, {
    if (current_index() < nrow(game_deck())) {
      next_index <- current_index() + 1
      next_card(game_deck()$Card[next_index])
      if (game_deck()$Rank[next_index] < game_deck()$Rank[current_index()]) {
        result("Correct! The next card is lower.")
      } else {
        result("Wrong! The next card is not lower.")
      }
      current_index(next_index)
      current_card(game_deck()$Card[next_index])
    } else {
      result("No more cards in the deck!")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

