#!/usr/bin/env Rscript

# Test Shiny app outside of renv environment
# This will use system-installed packages instead of renv

# Deactivate renv temporarily
Sys.setenv(RENV_ACTIVATE = "FALSE")

cat("Testing Shiny outside renv environment...\n")

# Try with absolute minimal Shiny app
library(shiny)

ui <- fluidPage(
  titlePanel("Minimal Test - No renv"),
  p("This is a test to isolate the JavaScript error."),
  textInput("test", "Type something:"),
  verbatimTextOutput("output")
)

server <- function(input, output) {
  output$output <- renderText({
    if(is.null(input$test) || input$test == "") {
      "No input yet"
    } else {
      paste("You typed:", input$test)
    }
  })
}

cat("Starting minimal app...\n")
shinyApp(ui = ui, server = server)