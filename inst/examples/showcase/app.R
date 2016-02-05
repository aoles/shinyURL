library(shinyURL)

shinyApp(
  ui = fluidPage(
    titlePanel("Old Faithful Geyser Data"),
    sidebarLayout(
      sidebarPanel(
        selectInput("var", "Variable:", list(`Eruption time` = "eruptions", `Waiting time` = "waiting")),
        sliderInput("bins", "Number of bins:", min = 1,  max = 50, value = 30),
        shinyURL.ui()
      ),
      mainPanel(
        tabsetPanel(id = "tab", 
                    tabPanel("Plot", plotOutput("plot")), 
                    tabPanel("Summary", verbatimTextOutput("summary"))
        )
      )
    )
  ),
  server = function(input, output) {
    shinyURL.server()
    data <- reactive(faithful[, input$var])
    output$plot <- renderPlot({
      x <- data()
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    output$summary <- renderPrint(summary(data()))
  }
)
