# Health Insurance Fraud Detection Dashboard - Fixed Version
# MediGuard Analytics - Interactive Claims Analysis

library(shiny)
library(bslib)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(visNetwork)

# Load data
claims_data <- read_csv("data/synthetic-claims.csv", comment = "#")
provider_summary <- read_csv("data/synthetic-provider-summary.csv", comment = "#")
member_summary <- read_csv("data/synthetic-member-summary.csv", comment = "#")

# Convert date columns
claims_data$service_date <- as.Date(claims_data$service_date)
claims_data$submission_date <- as.Date(claims_data$submission_date)

# Network Analysis Functions
prepare_network_data <- function(claims_df, min_connections = 2) {
  # Create provider-member connections with fraud indicators
  provider_member <- claims_df |>
    group_by(provider_id, member_id) |>
    summarise(
      num_claims = n(),
      total_amount = sum(billed_amount),
      fraud_claims = sum(is_fraud),
      fraud_rate = mean(is_fraud),
      .groups = "drop"
    ) |>
    filter(num_claims >= min_connections)
  
  # Create provider-provider connections (shared members)
  provider_connections <- claims_df |>
    inner_join(claims_df, by = "member_id", suffix = c("_1", "_2")) |>
    filter(provider_id_1 < provider_id_2) |>
    group_by(provider_id_1, provider_id_2) |>
    summarise(
      shared_members = n_distinct(member_id),
      shared_fraud_claims = sum(is_fraud_1 | is_fraud_2),
      .groups = "drop"
    ) |>
    filter(shared_members >= min_connections)
  
  list(
    provider_member = provider_member,
    provider_connections = provider_connections
  )
}

create_fraud_network <- function(claims_df, network_type = "provider-member", threshold = 0.05) {
  # Filter for suspicious activity
  suspicious_claims <- claims_df |>
    filter(is_fraud == TRUE | billed_amount > quantile(billed_amount, 0.95))
  
  if (network_type == "provider-member") {
    # Create bipartite network of providers and members
    edges <- suspicious_claims |>
      group_by(provider_id, member_id) |>
      summarise(
        weight = n(),
        total_amount = sum(billed_amount),
        fraud_count = sum(is_fraud),
        .groups = "drop"
      )
    
    # Create node data with unique IDs
    provider_nodes <- suspicious_claims |>
      group_by(provider_id) |>
      summarise(
        type = "Provider",
        label = provider_id,
        fraud_rate = mean(is_fraud),
        total_claims = n(),
        .groups = "drop"
      ) |>
      mutate(id = paste0("P_", provider_id))  # Prefix with P_ for providers
    
    member_nodes <- suspicious_claims |>
      group_by(member_id) |>
      summarise(
        type = "Member",
        label = member_id,
        fraud_rate = mean(is_fraud),
        total_claims = n(),
        .groups = "drop"
      ) |>
      mutate(id = paste0("M_", member_id))  # Prefix with M_ for members
    
    nodes <- bind_rows(provider_nodes, member_nodes)
    
    # Prepare for visNetwork with prefixed IDs
    edges_vis <- edges |>
      mutate(
        from = paste0("P_", provider_id),
        to = paste0("M_", member_id),
        value = weight
      ) |>
      select(from, to, value)
    
  } else {
    # Provider-provider network based on shared suspicious members
    provider_pairs <- suspicious_claims |>
      select(member_id, provider_id) |>
      inner_join(
        suspicious_claims |> select(member_id, provider_id),
        by = "member_id",
        suffix = c("_1", "_2")
      ) |>
      filter(provider_id_1 < provider_id_2) |>
      group_by(provider_id_1, provider_id_2) |>
      summarise(
        shared_members = n_distinct(member_id),
        .groups = "drop"
      ) |>
      filter(shared_members >= 2)
    
    # Node data for providers
    nodes <- suspicious_claims |>
      group_by(provider_id) |>
      summarise(
        type = "Provider",
        label = provider_id,
        fraud_rate = mean(is_fraud),
        total_claims = n(),
        specialty = first(specialty),
        .groups = "drop"
      ) |>
      rename(id = provider_id)
    
    edges_vis <- provider_pairs |>
      rename(from = provider_id_1, to = provider_id_2, value = shared_members)
  }
  
  # Add visual properties to nodes and ensure unique IDs
  nodes <- nodes |>
    distinct(id, .keep_all = TRUE) |>  # Ensure unique IDs
    mutate(
      color = case_when(
        fraud_rate > 0.5 ~ "#FF6B6B",
        fraud_rate > 0.2 ~ "#FFB700",
        fraud_rate > 0.1 ~ "#FFA500",
        TRUE ~ "#00A19C"
      ),
      size = sqrt(total_claims) * 5,
      title = paste0(
        "<b>", label, "</b><br>",
        "Type: ", type, "<br>",
        "Claims: ", total_claims, "<br>",
        "Fraud Rate: ", round(fraud_rate * 100, 1), "%"
      ),
      font.size = 12
    )
  
  # Filter edges to only include valid nodes
  valid_node_ids <- nodes$id
  edges_vis <- edges_vis |>
    filter(from %in% valid_node_ids & to %in% valid_node_ids)
  
  list(nodes = nodes, edges = edges_vis)
}

# Define brand colors
brand_colors <- list(
  primary = "#003D7A",
  secondary = "#00A19C",
  success = "#00A86B",
  warning = "#FFB700",
  danger = "#FF6B6B",
  info = "#5EB3E4"
)

# Create custom theme from brand
custom_theme <- bs_theme(
  version = 5,
  primary = brand_colors$primary,
  secondary = brand_colors$secondary,
  success = brand_colors$success,
  warning = brand_colors$warning,
  danger = brand_colors$danger,
  info = brand_colors$info,
  base_font = font_google("Inter"),
  heading_font = font_google("Roboto Slab"),
  code_font = font_google("Fira Code")
)

# Simple query processing function (non-reactive)
process_simple_query <- function(query_text) {
  query_lower <- tolower(query_text)
  
  result <- list(text = "", data = NULL)
  
  # Overall fraud rate queries
  if (grepl("overall fraud rate|total fraud rate|fraud rate", query_lower)) {
    fraud_rate <- mean(claims_data$is_fraud) * 100
    total_claims <- nrow(claims_data)
    fraud_claims <- sum(claims_data$is_fraud)
    
    result$text <- paste0("The overall fraud rate is ", round(fraud_rate, 2), "% (", 
                         format(fraud_claims, big.mark = ","), " fraudulent claims out of ", 
                         format(total_claims, big.mark = ","), " total claims).")
  }
  
  # Specialty fraud rate queries
  else if (grepl("specialty.*fraud|fraud.*specialty", query_lower)) {
    specialty_fraud <- claims_data |>
      group_by(specialty) |>
      summarise(
        total_claims = n(),
        fraud_claims = sum(is_fraud),
        fraud_rate = mean(is_fraud) * 100,
        .groups = "drop"
      ) |>
      arrange(desc(fraud_rate)) |>
      head(10)
    
    top_specialty <- specialty_fraud$specialty[1]
    top_rate <- round(specialty_fraud$fraud_rate[1], 2)
    
    result$text <- paste0("The specialty with the highest fraud rate is ", top_specialty, 
                         " at ", top_rate, "%. Here are the top 10 specialties by fraud rate:")
    result$data <- specialty_fraud
  }
  
  # High amount claims queries
  else if (grepl("claims over|claims above|high amount|expensive claims", query_lower)) {
    # Extract amount if specified
    amount_match <- regexpr("\\$?[0-9,]+", query_text)
    if (amount_match > 0) {
      amount_str <- regmatches(query_text, amount_match)
      amount_str <- gsub("[\\$,]", "", amount_str)
      threshold <- as.numeric(amount_str)
    } else {
      threshold <- 5000  # Default threshold
    }
    
    high_claims <- claims_data |>
      filter(billed_amount > threshold) |>
      select(claim_id, provider_id, member_id, service_date, billed_amount, 
             is_fraud, fraud_type, specialty) |>
      arrange(desc(billed_amount)) |>
      head(20)
    
    fraud_count <- sum(high_claims$is_fraud)
    total_count <- nrow(high_claims)
    
    result$text <- paste0("Found ", format(total_count, big.mark = ","), 
                         " claims over $", format(threshold, big.mark = ","), 
                         ". Of these, ", fraud_count, " (", 
                         round(fraud_count/total_count*100, 1), "%) are fraudulent.")
    result$data <- high_claims
  }
  
  # Common fraud types queries
  else if (grepl("common fraud|fraud type|type.*fraud", query_lower)) {
    fraud_types <- claims_data |>
      filter(is_fraud == TRUE, fraud_type != "none") |>
      count(fraud_type) |>
      arrange(desc(n)) |>
      mutate(percentage = round(n / sum(n) * 100, 1))
    
    if (nrow(fraud_types) > 0) {
      top_type <- fraud_types$fraud_type[1]
      top_count <- fraud_types$n[1]
      top_pct <- fraud_types$percentage[1]
      
      result$text <- paste0("The most common fraud type is '", top_type, 
                           "' with ", format(top_count, big.mark = ","), 
                           " cases (", top_pct, "% of all fraud cases).")
      result$data <- fraud_types
    }
  }
  
  # State-based fraud queries
  else if (grepl("state.*fraud|fraud.*state|which state", query_lower)) {
    state_fraud <- claims_data |>
      group_by(state) |>
      summarise(
        total_claims = n(),
        fraud_claims = sum(is_fraud),
        fraud_rate = mean(is_fraud) * 100,
        .groups = "drop"
      ) |>
      arrange(desc(fraud_claims)) |>
      head(10)
    
    top_state <- state_fraud$state[1]
    top_count <- state_fraud$fraud_claims[1]
    
    result$text <- paste0("The state with the most fraudulent claims is ", top_state, 
                         " with ", format(top_count, big.mark = ","), " fraud cases.")
    result$data <- state_fraud
  }
  
  # Default response
  else {
    result$text <- paste0("I can help you analyze the claims data. Try asking about: ",
                         "fraud rates, specialties, high-value claims, fraud types, or states.")
  }
  
  return(result)
}

# UI with simplified chat
ui <- page_navbar(
  title = "MediGuard Fraud Detection Dashboard",
  theme = custom_theme,
  
  nav_panel(
    "Overview",
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Key Metrics"),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "Total Claims",
            value = format(nrow(claims_data), big.mark = ","),
            showcase = bsicons::bs_icon("file-text"),
            theme = "primary"
          ),
          value_box(
            title = "Fraud Rate",
            value = paste0(round(mean(claims_data$is_fraud) * 100, 2), "%"),
            showcase = bsicons::bs_icon("exclamation-triangle"),
            theme = "danger"
          ),
          value_box(
            title = "Total Billed",
            value = paste0("$", format(sum(claims_data$billed_amount), big.mark = ",", scientific = FALSE)),
            showcase = bsicons::bs_icon("currency-dollar"),
            theme = "success"
          ),
          value_box(
            title = "Providers",
            value = format(length(unique(claims_data$provider_id)), big.mark = ","),
            showcase = bsicons::bs_icon("hospital"),
            theme = "info"
          )
        )
      ),
      card(
        card_header("Claims Over Time"),
        plotlyOutput("claims_timeline", height = "400px")
      ),
      card(
        card_header("Fraud Distribution"),
        layout_columns(
          col_widths = c(6, 6),
          plotlyOutput("fraud_by_type", height = "350px"),
          plotlyOutput("fraud_by_specialty", height = "350px")
        )
      )
    )
  ),
  
  nav_panel(
    "Provider Analysis",
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Provider Risk Matrix"),
        plotlyOutput("provider_risk_matrix", height = "500px")
      ),
      card(
        card_header("Filters"),
        selectInput(
          "provider_specialty",
          "Specialty",
          choices = c("All", unique(claims_data$specialty)),
          selected = "All"
        ),
        selectInput(
          "provider_region",
          "Region",
          choices = c("All", unique(claims_data$region)),
          selected = "All"
        ),
        sliderInput(
          "min_claims",
          "Minimum Claims",
          min = 1,
          max = 100,
          value = 10,
          step = 5
        ),
        actionButton("apply_filters", "Apply Filters", class = "btn-primary")
      )
    ),
    card(
      card_header("High-Risk Providers"),
      DTOutput("high_risk_providers")
    )
  ),
  
  nav_panel(
    "Claims Analysis",
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Claims Data Explorer"),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            selectInput(
              "claim_type_filter",
              "Claim Type",
              choices = c("All", unique(claims_data$claim_type)),
              selected = "All"
            ),
            selectInput(
              "fraud_filter",
              "Fraud Status",
              choices = c("All", "Normal", "Fraudulent"),
              selected = "All"
            ),
            dateRangeInput(
              "date_range",
              "Service Date Range",
              start = min(claims_data$service_date),
              end = max(claims_data$service_date)
            ),
            actionButton("filter_claims", "Filter", class = "btn-primary")
          ),
          DTOutput("claims_table")
        )
      )
    ),
    card(
      card_header("Claim Amount Distribution"),
      plotlyOutput("amount_distribution", height = "400px")
    )
  ),
  
  nav_panel(
    "Geographic Analysis",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Fraud Rate by State"),
        plotlyOutput("fraud_by_state", height = "450px")
      ),
      card(
        card_header("Claims Volume by State"),
        plotlyOutput("claims_by_state", height = "450px")
      )
    ),
    card(
      card_header("State-Level Statistics"),
      DTOutput("state_stats")
    )
  ),
  
  nav_panel(
    "Network Analysis",
    layout_columns(
      col_widths = c(9, 3),
      card(
        card_header("Fraud Network Visualization"),
        card_body(
          visNetworkOutput("fraud_network", height = "600px")
        )
      ),
      card(
        card_header("Network Controls"),
        card_body(
          selectInput(
            "network_type",
            "Network Type",
            choices = c(
              "Provider-Member Connections" = "provider-member",
              "Provider-Provider Links" = "provider-provider"
            ),
            selected = "provider-member"
          ),
          sliderInput(
            "network_threshold",
            "Min Connections",
            min = 1,
            max = 10,
            value = 2,
            step = 1
          ),
          checkboxInput(
            "show_fraud_only",
            "Show Fraud Cases Only",
            value = TRUE
          ),
          actionButton("update_network", "Update Network", class = "btn-primary"),
          hr(),
          h6("Network Metrics"),
          tableOutput("network_metrics")
        )
      )
    ),
    card(
      card_header("Suspicious Patterns Detected"),
      DTOutput("suspicious_patterns")
    )
  ),
  
  # Simplified Query Chat Tab
  nav_panel(
    "Query Assistant",
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Ask Questions About Claims Data"),
        card_body(
          h5("Sample Questions:"),
          tags$ul(
            tags$li("What is the overall fraud rate?"),
            tags$li("Which specialty has the highest fraud rate?"),
            tags$li("Show me claims over $5000"),
            tags$li("What are the most common fraud types?"),
            tags$li("Which state has the most fraudulent claims?")
          ),
          br(),
          textInput(
            "query_input", 
            "Enter your question:",
            placeholder = "Type your question here..."
          ),
          actionButton("submit_query", "Submit Query", class = "btn-primary"),
          br(), br(),
          wellPanel(
            h6("Response:"),
            verbatimTextOutput("query_response")
          )
        )
      ),
      card(
        card_header("Query Results"),
        DTOutput("query_results_table")
      )
    )
  ),
  
  nav_panel(
    "About",
    card(
      card_header("About This Dashboard"),
      card_body(
        h4("MediGuard Analytics - Fraud Detection System"),
        p("This dashboard provides comprehensive analytics for health insurance claims fraud detection."),
        br(),
        h5("Key Features:"),
        tags$ul(
          tags$li("Real-time fraud detection and risk scoring"),
          tags$li("Provider performance monitoring"),
          tags$li("Network link analysis to detect fraud rings"),
          tags$li("Geographic fraud pattern analysis"),
          tags$li("Claims data exploration and filtering"),
          tags$li("Natural language query assistant")
        ),
        br(),
        h5("Data Notice:"),
        p(class = "text-warning", 
          "All data presented in this dashboard is synthetic and generated for demonstration purposes only. 
          It does not represent real claims, providers, or members."),
        br(),
        h5("Technology Stack:"),
        tags$ul(
          tags$li("R Shiny for interactive visualization"),
          tags$li("Python for machine learning models"),
          tags$li("Posit Connect for deployment"),
          tags$li("Quarto for reporting")
        )
      )
    )
  )
)

# Server with simplified chat logic
server <- function(input, output, session) {
  
  # Reactive data for claims filtering
  filtered_claims <- reactive({
    data <- claims_data
    
    if (input$fraud_filter == "Normal") {
      data <- data |> filter(is_fraud == FALSE)
    } else if (input$fraud_filter == "Fraudulent") {
      data <- data |> filter(is_fraud == TRUE)
    }
    
    if (input$claim_type_filter != "All") {
      data <- data |> filter(claim_type == input$claim_type_filter)
    }
    
    data <- data |> filter(
      service_date >= input$date_range[1],
      service_date <= input$date_range[2]
    )
    
    data
  }) |> bindEvent(input$filter_claims, ignoreNULL = FALSE)
  
  # Reactive data for provider filtering
  filtered_providers <- reactive({
    data <- claims_data
    
    if (input$provider_specialty != "All") {
      data <- data |> filter(specialty == input$provider_specialty)
    }
    
    if (input$provider_region != "All") {
      data <- data |> filter(region == input$provider_region)
    }
    
    provider_stats <- data |>
      group_by(provider_id, specialty, region) |>
      summarise(
        total_claims = n(),
        fraud_claims = sum(is_fraud),
        fraud_rate = mean(is_fraud),
        avg_billed = mean(billed_amount),
        .groups = "drop"
      ) |>
      filter(total_claims >= input$min_claims)
    
    provider_stats
  }) |> bindEvent(input$apply_filters, ignoreNULL = FALSE)
  
  # Simple Query Processing
  observeEvent(input$submit_query, {
    if (nchar(trimws(input$query_input)) == 0) return()
    
    # Process query
    result <- process_simple_query(input$query_input)
    
    # Update text response
    output$query_response <- renderText({
      result$text
    })
    
    # Update table if data exists
    if (!is.null(result$data)) {
      output$query_results_table <- renderDT({
        datatable(result$data,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
      })
    } else {
      output$query_results_table <- renderDT({
        datatable(data.frame(Message = "No tabular data for this query"),
                  options = list(searching = FALSE, info = FALSE),
                  rownames = FALSE)
      })
    }
  })
  
  # Overview tab outputs
  output$claims_timeline <- renderPlotly({
    monthly_data <- claims_data |>
      mutate(month = floor_date(service_date, "month")) |>
      group_by(month) |>
      summarise(
        total_claims = n(),
        fraud_claims = sum(is_fraud),
        fraud_rate = mean(is_fraud) * 100,
        .groups = "drop"
      )
    
    p <- plot_ly(monthly_data, x = ~month) |>
      add_bars(y = ~total_claims, name = "Total Claims", 
               marker = list(color = brand_colors$primary)) |>
      add_lines(y = ~fraud_rate * 10, name = "Fraud Rate (x10)", 
                line = list(color = brand_colors$danger), yaxis = "y2") |>
      layout(
        title = "Monthly Claims and Fraud Rate",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Number of Claims"),
        yaxis2 = list(
          title = "Fraud Rate (%)",
          overlaying = "y",
          side = "right",
          range = c(0, max(monthly_data$fraud_rate) * 1.2)
        ),
        hovermode = "x unified"
      )
    
    p
  })
  
  output$fraud_by_type <- renderPlotly({
    fraud_type_data <- claims_data |>
      filter(is_fraud == TRUE) |>
      count(fraud_type) |>
      arrange(desc(n))
    
    plot_ly(fraud_type_data, 
            x = ~fraud_type, 
            y = ~n,
            type = "bar",
            marker = list(color = brand_colors$danger)) |>
      layout(
        title = "Fraud by Type",
        xaxis = list(title = "Fraud Type"),
        yaxis = list(title = "Count")
      )
  })
  
  output$fraud_by_specialty <- renderPlotly({
    specialty_fraud <- claims_data |>
      group_by(specialty) |>
      summarise(
        fraud_rate = mean(is_fraud) * 100,
        .groups = "drop"
      ) |>
      arrange(desc(fraud_rate)) |>
      head(10)
    
    plot_ly(specialty_fraud,
            x = ~reorder(specialty, fraud_rate),
            y = ~fraud_rate,
            type = "bar",
            marker = list(color = brand_colors$warning)) |>
      layout(
        title = "Top 10 Specialties by Fraud Rate",
        xaxis = list(title = ""),
        yaxis = list(title = "Fraud Rate (%)"),
        margin = list(b = 120)
      )
  })
  
  # Provider Analysis outputs
  output$provider_risk_matrix <- renderPlotly({
    provider_data <- filtered_providers()
    
    plot_ly(provider_data,
            x = ~fraud_rate * 100,
            y = ~avg_billed,
            size = ~total_claims,
            color = ~fraud_rate,
            colors = c(brand_colors$success, brand_colors$warning, brand_colors$danger),
            text = ~paste("Provider:", provider_id,
                         "<br>Claims:", total_claims,
                         "<br>Fraud Rate:", round(fraud_rate * 100, 2), "%",
                         "<br>Avg Billed:", round(avg_billed, 2)),
            hovertemplate = "%{text}<extra></extra>",
            type = "scatter",
            mode = "markers") |>
      layout(
        title = "Provider Risk Assessment",
        xaxis = list(title = "Fraud Rate (%)"),
        yaxis = list(title = "Average Billed Amount ($)"),
        showlegend = FALSE
      )
  })
  
  output$high_risk_providers <- renderDT({
    provider_data <- filtered_providers() |>
      filter(fraud_rate > 0.05) |>
      arrange(desc(fraud_rate)) |>
      select(provider_id, specialty, region, total_claims, fraud_claims, fraud_rate, avg_billed)
    
    datatable(provider_data,
              options = list(pageLength = 10),
              rownames = FALSE) |>
      formatPercentage("fraud_rate", 2) |>
      formatCurrency("avg_billed")
  })
  
  # Claims Analysis outputs
  output$claims_table <- renderDT({
    data <- filtered_claims() |>
      select(claim_id, member_id, provider_id, service_date, 
             claim_type, billed_amount, is_fraud, fraud_type)
    
    datatable(data,
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE) |>
      formatCurrency("billed_amount") |>
      formatStyle("is_fraud",
                 backgroundColor = styleEqual(TRUE, "rgba(255, 107, 107, 0.2)"))
  })
  
  output$amount_distribution <- renderPlotly({
    data <- filtered_claims()
    
    plot_ly(alpha = 0.6) |>
      add_histogram(x = data$billed_amount[data$is_fraud == FALSE],
                   name = "Normal Claims",
                   marker = list(color = brand_colors$primary)) |>
      add_histogram(x = data$billed_amount[data$is_fraud == TRUE],
                   name = "Fraudulent Claims",
                   marker = list(color = brand_colors$danger)) |>
      layout(
        title = "Billed Amount Distribution",
        xaxis = list(title = "Billed Amount ($)", range = c(0, 20000)),
        yaxis = list(title = "Frequency"),
        barmode = "overlay"
      )
  })
  
  # Geographic Analysis outputs
  output$fraud_by_state <- renderPlotly({
    state_fraud <- claims_data |>
      group_by(state) |>
      summarise(
        fraud_rate = mean(is_fraud) * 100,
        total_claims = n(),
        .groups = "drop"
      ) |>
      arrange(desc(fraud_rate))
    
    plot_ly(state_fraud,
            x = ~reorder(state, -fraud_rate),
            y = ~fraud_rate,
            type = "bar",
            marker = list(color = ~fraud_rate,
                         colorscale = list(c(0, brand_colors$success),
                                         c(0.5, brand_colors$warning),
                                         c(1, brand_colors$danger)))) |>
      layout(
        title = "Fraud Rate by State",
        xaxis = list(title = "State"),
        yaxis = list(title = "Fraud Rate (%)"),
        showlegend = FALSE
      )
  })
  
  output$claims_by_state <- renderPlotly({
    state_volume <- claims_data |>
      count(state) |>
      arrange(desc(n))
    
    plot_ly(state_volume,
            x = ~reorder(state, -n),
            y = ~n,
            type = "bar",
            marker = list(color = brand_colors$info)) |>
      layout(
        title = "Claims Volume by State",
        xaxis = list(title = "State"),
        yaxis = list(title = "Number of Claims")
      )
  })
  
  output$state_stats <- renderDT({
    state_summary <- claims_data |>
      group_by(state) |>
      summarise(
        total_claims = n(),
        fraud_claims = sum(is_fraud),
        fraud_rate = mean(is_fraud),
        total_billed = sum(billed_amount),
        avg_billed = mean(billed_amount),
        .groups = "drop"
      ) |>
      arrange(desc(fraud_rate))
    
    datatable(state_summary,
              options = list(pageLength = 10),
              rownames = FALSE) |>
      formatPercentage("fraud_rate", 2) |>
      formatCurrency(c("total_billed", "avg_billed"))
  })
  
  # Network Analysis outputs
  network_data <- reactive({
    tryCatch({
      # Filter data based on fraud checkbox
      data <- if(input$show_fraud_only) {
        claims_data |> filter(is_fraud == TRUE | billed_amount > quantile(billed_amount, 0.95))
      } else {
        claims_data
      }
      
      # Check if we have data
      if (nrow(data) == 0) {
        return(list(nodes = data.frame(), edges = data.frame()))
      }
      
      create_fraud_network(
        data,
        network_type = input$network_type,
        threshold = input$network_threshold
      )
    }, error = function(e) {
      message("Error in network_data: ", e$message)
      return(list(nodes = data.frame(), edges = data.frame()))
    })
  }) |> bindEvent(input$update_network, ignoreNULL = FALSE)
  
  output$fraud_network <- renderVisNetwork({
    tryCatch({
      net_data <- network_data()
      
      # Check if we have valid data
      if (is.null(net_data) || nrow(net_data$nodes) == 0) {
        # Return empty network with message
        nodes <- data.frame(id = 1, label = "No data to display", title = "No fraudulent claims found")
        edges <- data.frame(from = integer(), to = integer())
        
        visNetwork(nodes, edges) |>
          visOptions(
            nodesIdSelection = FALSE
          )
      } else {
        # Create the network visualization
        visNetwork(net_data$nodes, net_data$edges) |>
          visOptions(
            highlightNearest = list(enabled = TRUE, hover = TRUE),
            nodesIdSelection = TRUE,
            selectedBy = "type"
          ) |>
          visPhysics(
            stabilization = list(enabled = TRUE, iterations = 100),
            barnesHut = list(gravitationalConstant = -8000, springConstant = 0.04)
          ) |>
          visLayout(randomSeed = 42) |>
          visInteraction(
            navigationButtons = TRUE,
            dragNodes = TRUE,
            dragView = TRUE,
            zoomView = TRUE
          ) |>
          visEdges(
            smooth = list(type = "continuous"),
            color = list(color = "#CCCCCC", highlight = brand_colors$primary)
          ) |>
          visLegend(
            enabled = TRUE,
            position = "right",
            width = 0.2
          )
      }
    }, error = function(e) {
      # Display error message in the network area
      nodes <- data.frame(
        id = 1, 
        label = "Error loading network", 
        title = paste("Error:", e$message)
      )
      edges <- data.frame(from = integer(), to = integer())
      
      visNetwork(nodes, edges) |>
        visOptions(nodesIdSelection = FALSE)
    })
  })
  
  output$network_metrics <- renderTable({
    net_data <- network_data()
    
    # Calculate basic network metrics without igraph
    if (nrow(net_data$edges) > 0) {
      num_nodes <- nrow(net_data$nodes)
      num_edges <- nrow(net_data$edges)
      
      # Calculate average degree
      degree_data <- net_data$edges |>
        select(from, to) |>
        pivot_longer(cols = c(from, to), values_to = "node") |>
        count(node) |>
        pull(n)
      
      avg_degree <- mean(degree_data)
      
      # Simple density calculation
      max_edges <- num_nodes * (num_nodes - 1) / 2
      density <- num_edges / max_edges
      
      metrics <- data.frame(
        Metric = c("Nodes", "Edges", "Density", "Avg Degree"),
        Value = c(
          num_nodes,
          num_edges,
          round(density, 3),
          round(avg_degree, 2)
        )
      )
    } else {
      metrics <- data.frame(
        Metric = c("Nodes", "Edges"),
        Value = c(nrow(net_data$nodes), 0)
      )
    }
    
    metrics
  }, spacing = "xs", width = "100%")
  
  output$suspicious_patterns <- renderDT({
    net_data <- network_data()
    
    # Identify suspicious patterns
    if (input$network_type == "provider-member") {
      # Find high-risk provider-member relationships
      suspicious <- claims_data |>
        group_by(provider_id, member_id) |>
        summarise(
          total_claims = n(),
          fraud_claims = sum(is_fraud),
          fraud_rate = mean(is_fraud),
          total_amount = sum(billed_amount),
          avg_amount = mean(billed_amount),
          max_amount = max(billed_amount),
          fraud_types = paste(unique(fraud_type[is_fraud == TRUE]), collapse = ", "),
          .groups = "drop"
        ) |>
        filter(
          (fraud_rate > 0.5 & total_claims >= 2) |  # High fraud rate with multiple claims
          (fraud_claims >= 1 & avg_amount > 1000) |   # Any fraud with high amounts
          (max_amount > 5000)                         # Very high single claims
        ) |>
        arrange(desc(fraud_rate), desc(total_amount)) |>
        head(20)  # Show top 20 suspicious patterns
      
      # Add risk score
      suspicious <- suspicious |>
        mutate(
          risk_score = round(
            (fraud_rate * 40) + 
            (pmin(total_claims / 10, 1) * 20) + 
            (pmin(avg_amount / 2000, 1) * 40), 
            1
          ),
          fraud_types = ifelse(fraud_types == "", "High Amount", fraud_types)
        ) |>
        select(provider_id, member_id, total_claims, fraud_claims, fraud_rate, 
               total_amount, avg_amount, max_amount, fraud_types, risk_score)
      
    } else {
      # Find provider pairs with suspicious connections
      provider_fraud_rates <- claims_data |>
        group_by(provider_id) |>
        summarise(provider_fraud_rate = mean(is_fraud), .groups = "drop")
      
      suspicious <- claims_data |>
        select(member_id, provider_id, billed_amount, is_fraud) |>
        inner_join(
          claims_data |> select(member_id, provider_id, billed_amount, is_fraud),
          by = "member_id",
          suffix = c("_1", "_2")
        ) |>
        filter(provider_id_1 < provider_id_2) |>
        group_by(provider_id_1, provider_id_2) |>
        summarise(
          shared_members = n_distinct(member_id),
          shared_fraud_cases = sum(is_fraud_1 | is_fraud_2),
          total_amount = sum(billed_amount_1 + billed_amount_2),
          avg_amount = mean(billed_amount_1 + billed_amount_2),
          .groups = "drop"
        ) |>
        left_join(provider_fraud_rates, by = c("provider_id_1" = "provider_id")) |>
        left_join(provider_fraud_rates, by = c("provider_id_2" = "provider_id"), suffix = c("_1", "_2")) |>
        filter(
          shared_members >= 3 |                                    # Multiple shared members
          (shared_fraud_cases >= 2 & shared_members >= 2) |        # Multiple fraud cases
          (avg_amount > 1500 & shared_members >= 2)                # High amounts
        ) |>
        mutate(
          combined_fraud_rate = (provider_fraud_rate_1 + provider_fraud_rate_2) / 2,
          risk_score = round(
            (shared_fraud_cases * 10) + 
            (shared_members * 5) + 
            (combined_fraud_rate * 50), 
            1
          )
        ) |>
        arrange(desc(risk_score)) |>
        head(15) |>
        select(provider_id_1, provider_id_2, shared_members, shared_fraud_cases, 
               total_amount, combined_fraud_rate, risk_score)
    }
    
    # Create datatable with appropriate formatting
    if (nrow(suspicious) > 0) {
      dt <- datatable(suspicious,
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE) |>
        formatStyle(
          columns = 1:ncol(suspicious),
          fontSize = '12px'
        )
      
      # Apply currency formatting to amount columns
      if ("total_amount" %in% names(suspicious)) {
        dt <- dt |> formatCurrency("total_amount", digits = 0)
      }
      if ("avg_amount" %in% names(suspicious)) {
        dt <- dt |> formatCurrency("avg_amount", digits = 0)
      }
      if ("max_amount" %in% names(suspicious)) {
        dt <- dt |> formatCurrency("max_amount", digits = 0)
      }
      
      # Apply percentage formatting to rate columns
      if ("fraud_rate" %in% names(suspicious)) {
        dt <- dt |> formatPercentage("fraud_rate", 1)
      }
      if ("combined_fraud_rate" %in% names(suspicious)) {
        dt <- dt |> formatPercentage("combined_fraud_rate", 1)
      }
      
      # Color code risk scores
      if ("risk_score" %in% names(suspicious)) {
        dt <- dt |> formatStyle(
          "risk_score",
          backgroundColor = styleInterval(
            cuts = c(25, 50, 75),
            values = c("white", "#fff3cd", "#ffeaa7", "#fd79a8")
          )
        )
      }
      
      dt
    } else {
      # Return empty table with message
      empty_data <- data.frame(Message = "No suspicious patterns detected with current filters")
      datatable(empty_data, 
                options = list(pageLength = 5, searching = FALSE, info = FALSE),
                rownames = FALSE)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)