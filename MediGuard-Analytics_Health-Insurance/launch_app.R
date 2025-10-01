#!/usr/bin/env Rscript

# Simple app launcher
cat("Starting MediGuard Fraud Detection Dashboard...\n")
cat("This may take a moment to load all the data and initialize...\n\n")

library(shiny)

# Run the app with better settings
options(shiny.port = 3838)
options(shiny.host = "0.0.0.0")  # Allow external connections
options(warn = -1)  # Suppress warnings during startup

shiny::runApp(
  appDir = ".",
  port = 3838,
  host = "0.0.0.0",
  launch.browser = FALSE,
  display.mode = "normal"
)