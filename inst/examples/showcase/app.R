library(shinyURL)

shinyApp(
  ui = fluidPage(
    titlePanel("Old Faithful Geyser Data"),
    sidebarLayout(
      sidebarPanel(
        selectInput("var", "Variable:", list(`Eruption time` = "eruptions", `Waiting time` = "waiting")),
        shinyURL.ui(),
        div(id = "description", includeMarkdown("description.md"))
      ),
      mainPanel(
        tabsetPanel(id = "tab", 
                    tabPanel("Plot",
                             plotOutput("plot"),
                             radioButtons(inputId = "bins",
                                         label = "Number of bins:",
                                         choices = c(10, 20, 35, 50),
                                         selected = 20,
                                         inline = TRUE),
                             checkboxInput(inputId = "density",
                                           label = strong("Show density estimate"),
                                           value = FALSE),
                             # Display this only if the density is shown
                             conditionalPanel(condition = "input.density == true",
                                              sliderInput(inputId = "bandwidth",
                                                          label = "Bandwidth adjustment:",
                                                          min = 0.2, max = 2, value = 1, step = 0.2)
                             )
                    ), 
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
      bins <- seq(min(x), max(x), length.out = as.integer(input$bins) + 1)
      hist(x,
           breaks = bins,
           probability = TRUE,
           col = 'darkgray',
           border = 'white',
           xlab = paste(input$var, "[minutes]"))
      if (input$density)
        lines(density(x, adjust = input$bandwidth), col = "red")
    })
    
    output$summary <- renderPrint(summary(data()))
    
  }
)
