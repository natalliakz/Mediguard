# Absolutely minimal Shiny app test
library(shiny)

ui <- fluidPage(
  h1("Basic Test"),
  textInput("name", "Enter name:"),
  textOutput("greeting")
)

server <- function(input, output) {
  output$greeting <- renderText({
    paste("Hello", input$name)
  })
}

shinyApp(ui = ui, server = server)