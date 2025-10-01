#!/usr/bin/env Rscript

cat("=== SHINY ERROR DIAGNOSTIC TOOL ===\n\n")

# First, let's check what browser you're using and clear any potential issues
cat("1. BROWSER CACHE CLEARING INSTRUCTIONS:\n")
cat("   - Press Ctrl+Shift+R (or Cmd+Shift+R on Mac) to hard refresh\n")
cat("   - Or clear browser cache completely\n")
cat("   - Try in incognito/private mode\n")
cat("   - Try a different browser (Firefox, Safari, Edge)\n\n")

cat("2. CHECKING R/SHINY ENVIRONMENT:\n")
library(shiny)
cat("   ✓ Shiny version:", as.character(packageVersion("shiny")), "\n")
cat("   ✓ R version:", R.version.string, "\n")

cat("\n3. CREATING ABSOLUTE MINIMAL TEST APP:\n")

# Create the most basic Shiny app possible
ui <- basicPage(
  h1("Ultra Basic Test"),
  p("If you can see this without errors, Shiny works."),
  tags$div(id = "test", "Test div")
)

server <- function(input, output) {
  # Literally nothing in server
}

cat("   ✓ App created\n")
cat("\n4. INSTRUCTIONS:\n")
cat("   - This app has NO JavaScript dependencies\n")
cat("   - It should load without ANY errors\n")
cat("   - If this fails, the issue is with your R/Shiny setup\n")
cat("   - If this works, the issue is with specific packages\n")
cat("\n5. STARTING TEST APP...\n")
cat("   URL: http://localhost:3838\n")
cat("   Press Ctrl+C to stop\n\n")

# Set options for debugging
options(shiny.trace = TRUE)
options(shiny.error = browser)

shinyApp(ui = ui, server = server)