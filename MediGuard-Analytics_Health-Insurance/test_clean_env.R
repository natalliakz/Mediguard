#!/usr/bin/env Rscript

# Create a completely isolated test environment
cat("=== TESTING IN CLEAN ENVIRONMENT ===\n")

# Create temp directory
temp_dir <- file.path(tempdir(), "shiny_test")
dir.create(temp_dir, showWarnings = FALSE)
cat("Test directory:", temp_dir, "\n")

# Copy only the essential data file
file.copy("data/synthetic-claims.csv", file.path(temp_dir, "claims.csv"))

# Create minimal app in clean environment
app_content <- '
library(shiny)
library(readr)

# Load data
claims <- read_csv("claims.csv", comment = "#", show_col_types = FALSE)

ui <- fluidPage(
  titlePanel("Clean Environment Test"),
  h3("Data Summary"),
  verbatimTextOutput("summary"),
  textInput("query", "Test input:"),
  textOutput("echo")
)

server <- function(input, output) {
  output$summary <- renderText({
    paste("Loaded", nrow(claims), "claims successfully")
  })
  
  output$echo <- renderText({
    if(is.null(input$query) || input$query == "") {
      "Enter something above"
    } else {
      paste("You entered:", input$query)
    }
  })
}

shinyApp(ui = ui, server = server)
'

# Write app to temp directory
writeLines(app_content, file.path(temp_dir, "app.R"))

# Change to temp directory and run
old_wd <- getwd()
setwd(temp_dir)

cat("Starting app in clean directory...\n")
cat("If this works, the issue is with files in your project directory\n")

source("app.R")