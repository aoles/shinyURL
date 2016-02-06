library(shinyURL)

variables = list(`Eruption duration` = "eruptions", `Waiting time` = "waiting")

extractLabel = function(x) {
  s <- strsplit(x, " ")[[1L]][2L]
  paste0(toupper(substring(s, 1L, 1L)), substring(s, 2L))
}

shinyApp(
  ui = fluidPage(theme = "style.css",
                 title = "shinyURL demo",
    h1("Old Faithful Geyser Data"),
    sidebarLayout(
      sidebarPanel(
        selectInput("var", "Variable:", variables),
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
                          min = 0.5, max = 1.5, value = 1, step = 0.1)
              )
            ), 
          tabPanel("Summary",
            h3(textOutput("label")),
            verbatimTextOutput("summary")
            )
          )
        )
      )
    ),
  
  server = function(input, output) {
    shinyURL.server()
    
    data <- reactive( faithful[, input$var] )
    
    variable <- reactive( names(which(variables==input$var)) )
    
    output$plot <- renderPlot({
      x <- data()
      var <- variable()
      hist(x,
           breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),
           probability = TRUE,
           col = 'skyblue',
           border = 'white',
           main = var,
           xlab = paste(extractLabel(var), "[minutes]"))
      if (input$density)
        lines(density(x, adjust = input$bandwidth), col = "red", lwd = 2)
    })
    
    output$label <- renderText(variable())
    
    output$summary <- renderPrint(summary(data()))
    
  }
)
