#!/usr/bin/env Rscript

# Launch the fixed MediGuard app without JavaScript errors
cat("Starting MediGuard Fraud Detection Dashboard (Fixed Version)...\n")
cat("This version has a simplified query assistant that avoids JavaScript conflicts.\n\n")

library(shiny)

# Run the fixed app
options(shiny.port = 3838)
options(shiny.host = "0.0.0.0")
options(warn = -1)

shiny::runApp(
  appFile = "app_fixed.R",
  port = 3838,
  host = "0.0.0.0", 
  launch.browser = FALSE,
  display.mode = "normal"
)