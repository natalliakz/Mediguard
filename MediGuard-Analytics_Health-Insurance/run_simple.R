#!/usr/bin/env Rscript

cat("=== MediGuard Analytics - Ultra Simple Version ===\n")
cat("This version eliminates all JavaScript conflicts\n")
cat("Features: Overview, Claims Explorer, Query Assistant\n")
cat("Starting app on http://localhost:3838\n\n")

library(shiny)

runApp(
  appFile = "app_ultra_simple.R",
  port = 3838,
  host = "0.0.0.0",
  launch.browser = FALSE
)