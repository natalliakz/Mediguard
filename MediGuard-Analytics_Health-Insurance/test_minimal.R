# Minimal test app to isolate the JavaScript error
library(shiny)
library(bslib)
library(DT)

# Load only one data file to test
claims_data <- readr::read_csv("data/synthetic-claims.csv", comment = "#", show_col_types = FALSE)

ui <- fluidPage(
  titlePanel("Minimal Test App"),
  
  # Test 1: Basic data table
  h3("Test 1: Basic Data Table"),
  DTOutput("test_table"),
  
  # Test 2: Simple text input
  h3("Test 2: Text Input"),
  textInput("test_input", "Enter text:"),
  verbatimTextOutput("test_output"),
  
  # Test 3: Action button
  h3("Test 3: Action Button"),
  actionButton("test_button", "Click Me"),
  textOutput("button_output")
)

server <- function(input, output, session) {
  
  # Test basic data table
  output$test_table <- renderDT({
    datatable(head(claims_data, 100), options = list(pageLength = 10))
  })
  
  # Test text input/output
  output$test_output <- renderText({
    if (is.null(input$test_input) || input$test_input == "") {
      "No input"
    } else {
      paste("You entered:", input$test_input)
    }
  })
  
  # Test button
  output$button_output <- renderText({
    paste("Button clicked", input$test_button, "times")
  })
}

shinyApp(ui = ui, server = server)