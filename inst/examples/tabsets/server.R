library(shiny)
library(shinyURL)

shinyServer(function(input, output, session) {
  shinyURL.server(session)
  
  # Reactive expression to generate the requested distribution.
  data <- reactive({
    dist <- switch(input$dist, norm = rnorm, unif = runif, exp = rexp, rnorm)
    dist(input$n)
  })
  
  # Generate a plot of the data.
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    hist(data(),  main = paste('r', dist, '(', n, ')', sep=''))
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })
  
})
