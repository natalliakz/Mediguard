# Ultra Simple MediGuard App - No JavaScript Conflicts
# Eliminates all potential sources of the 'replace' error

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# Load data with error handling
tryCatch({
  claims_data <- read_csv("data/synthetic-claims.csv", comment = "#", show_col_types = FALSE)
  claims_data$service_date <- as.Date(claims_data$service_date)
  claims_data$submission_date <- as.Date(claims_data$submission_date)
}, error = function(e) {
  stop("Error loading data: ", e$message)
})

ui <- navbarPage(
  "MediGuard Analytics",
  
  tabPanel("Overview",
    fluidRow(
      column(3,
        wellPanel(
          h4("Key Metrics"),
          p(strong("Total Claims:"), format(nrow(claims_data), big.mark = ",")),
          p(strong("Fraud Rate:"), paste0(round(mean(claims_data$is_fraud) * 100, 2), "%")),
          p(strong("Total Billed:"), paste0("$", format(sum(claims_data$billed_amount), big.mark = ",", scientific = FALSE))),
          p(strong("Providers:"), format(length(unique(claims_data$provider_id)), big.mark = ","))
        )
      ),
      column(9,
        plotOutput("fraud_trend", height = "400px")
      )
    )
  ),
  
  tabPanel("Claims Explorer",
    fluidRow(
      column(3,
        wellPanel(
          h4("Filters"),
          selectInput("claim_type", "Claim Type:",
                     choices = c("All", unique(claims_data$claim_type)),
                     selected = "All"),
          selectInput("fraud_status", "Fraud Status:",
                     choices = c("All", "Normal", "Fraudulent"),
                     selected = "All"),
          actionButton("apply_filter", "Apply Filter", class = "btn-primary")
        )
      ),
      column(9,
        h4("Claims Summary"),
        tableOutput("claims_summary")
      )
    )
  ),
  
  tabPanel("Query Assistant",
    fluidRow(
      column(6,
        wellPanel(
          h4("Ask About Claims Data"),
          p("Sample questions:"),
          tags$ul(
            tags$li("fraud rate"),
            tags$li("top specialty"),
            tags$li("high claims"),
            tags$li("by state")
          ),
          textInput("query", "Your question:", placeholder = "Type here..."),
          actionButton("ask", "Ask", class = "btn-success")
        )
      ),
      column(6,
        wellPanel(
          h4("Answer"),
          verbatimTextOutput("answer")
        )
      )
    ),
    fluidRow(
      column(12,
        h4("Data Results"),
        tableOutput("query_data")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Basic reactive for filtered data
  filtered_data <- reactive({
    data <- claims_data
    
    if (!is.null(input$claim_type) && input$claim_type != "All") {
      data <- data[data$claim_type == input$claim_type, ]
    }
    
    if (!is.null(input$fraud_status)) {
      if (input$fraud_status == "Normal") {
        data <- data[data$is_fraud == FALSE, ]
      } else if (input$fraud_status == "Fraudulent") {
        data <- data[data$is_fraud == TRUE, ]
      }
    }
    
    data
  }) %>% bindEvent(input$apply_filter, ignoreNULL = FALSE)
  
  # Simple fraud trend plot
  output$fraud_trend <- renderPlot({
    monthly_data <- claims_data %>%
      mutate(month = format(service_date, "%Y-%m")) %>%
      group_by(month) %>%
      summarise(
        total_claims = n(),
        fraud_rate = mean(is_fraud) * 100,
        .groups = "drop"
      ) %>%
      head(12)  # Limit to avoid too much data
    
    ggplot(monthly_data, aes(x = month, y = fraud_rate)) +
      geom_line(group = 1, color = "red", size = 1) +
      geom_point(color = "red", size = 2) +
      theme_minimal() +
      labs(title = "Fraud Rate Over Time", x = "Month", y = "Fraud Rate (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Claims summary table
  output$claims_summary <- renderTable({
    data <- filtered_data()
    
    summary_stats <- data %>%
      summarise(
        `Total Claims` = n(),
        `Fraudulent Claims` = sum(is_fraud),
        `Fraud Rate (%)` = round(mean(is_fraud) * 100, 2),
        `Average Amount ($)` = round(mean(billed_amount), 2),
        `Total Billed ($)` = format(sum(billed_amount), big.mark = ","),
        .groups = "drop"
      )
    
    # Convert to long format for better display
    data.frame(
      Metric = names(summary_stats),
      Value = as.character(t(summary_stats)[,1])
    )
  })
  
  # Simple query processor
  observeEvent(input$ask, {
    if (is.null(input$query) || input$query == "") return()
    
    query <- tolower(input$query)
    result_text <- ""
    result_data <- NULL
    
    if (grepl("fraud rate", query)) {
      fraud_rate <- round(mean(claims_data$is_fraud) * 100, 2)
      result_text <- paste("Overall fraud rate is", fraud_rate, "%")
      
    } else if (grepl("top specialty", query)) {
      top_specialty <- claims_data %>%
        group_by(specialty) %>%
        summarise(fraud_rate = mean(is_fraud) * 100, .groups = "drop") %>%
        arrange(desc(fraud_rate)) %>%
        head(5)
      
      result_text <- paste("Top specialty by fraud rate:", top_specialty$specialty[1])
      result_data <- top_specialty
      
    } else if (grepl("high claims", query)) {
      high_claims <- claims_data %>%
        filter(billed_amount > 2000) %>%
        select(claim_id, provider_id, billed_amount, is_fraud) %>%
        head(10)
      
      result_text <- paste("Found", nrow(high_claims), "claims over $2000")
      result_data <- high_claims
      
    } else if (grepl("by state", query)) {
      by_state <- claims_data %>%
        group_by(state) %>%
        summarise(
          total_claims = n(),
          fraud_rate = round(mean(is_fraud) * 100, 2),
          .groups = "drop"
        ) %>%
        arrange(desc(fraud_rate)) %>%
        head(10)
      
      result_text <- paste("Top state by fraud rate:", by_state$state[1])
      result_data <- by_state
      
    } else {
      result_text <- "Try asking about: fraud rate, top specialty, high claims, or by state"
    }
    
    # Update outputs
    output$answer <- renderText({ result_text })
    
    output$query_data <- renderTable({
      if (!is.null(result_data)) {
        result_data
      } else {
        data.frame(Message = "No data to display")
      }
    })
  })
}

shinyApp(ui = ui, server = server)