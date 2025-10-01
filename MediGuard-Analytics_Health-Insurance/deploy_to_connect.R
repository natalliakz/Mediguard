#!/usr/bin/env Rscript

# Clean deployment script for MediGuard to Posit Connect

cat("=== MediGuard Deployment to Posit Connect ===\n\n")

# Load required library
library(rsconnect)

# Deployment configuration
app_name <- "MediGuard-Fraud-Detection-Chat"
app_title <- "MediGuard Analytics - Health Insurance Fraud Detection with Chat"

cat("1. Validating deployment readiness...\n")

# Check all required files
required_files <- c(
  "app.R",
  "renv.lock", 
  "_brand.yml",
  "data/synthetic-claims.csv",
  "data/synthetic-provider-summary.csv", 
  "data/synthetic-member-summary.csv"
)

for (file in required_files) {
  if (!file.exists(file)) {
    stop("ERROR: Required file not found: ", file)
  }
}

cat("✓ All required files present\n")

cat("\n2. Testing app functionality...\n")
tryCatch({
  source("app.R", local = TRUE)
  cat("✓ App loads without errors\n")
}, error = function(e) {
  stop("ERROR: App failed to load - ", e$message)
})

cat("\n3. Preparing deployment...\n")

# Clean up any temporary files that shouldn't be deployed
temp_files <- c("manifest.json", "deploy.R", "deploy_to_connect.R")
file.remove(temp_files[file.exists(temp_files)])

cat("✓ Cleanup completed\n")

cat("\n4. Deploying to Posit Connect...\n")
cat("This may take several minutes...\n\n")

# Deploy the app
tryCatch({
  rsconnect::deployApp(
    appDir = ".",
    appFiles = c(
      "app.R",
      "renv.lock",
      "_brand.yml",
      "data/"
    ),
    appName = app_name,
    appTitle = app_title,
    launch.browser = FALSE,
    forceUpdate = TRUE
  )
  
  cat("\n✅ SUCCESS: App deployed successfully!\n")
  cat("📱 Check your Posit Connect dashboard for the deployment URL\n")
  
}, error = function(e) {
  cat("\n❌ DEPLOYMENT FAILED:\n")
  cat("Error:", e$message, "\n\n")
  cat("💡 Troubleshooting tips:\n")
  cat("- Ensure you're logged into Posit Connect: rsconnect::showLogs()\n")
  cat("- Check package compatibility with Connect server\n")
  cat("- Verify data files are not too large\n")
  cat("- Try deploying without chat functionality first\n")
})