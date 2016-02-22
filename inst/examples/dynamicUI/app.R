library(shiny)
library(shinyURL)

# Define UI
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Dynamic User Interface"),
   
   # Sidebar with select inputs for data set and variables
   sidebarLayout(
      sidebarPanel(
        selectInput("dataset", "Dataset",
                    choices = list(Choose = "", "rock", "pressure", "cars")),
        uiOutput("varxInput"),
        uiOutput("varyInput"),
        shinyURL.ui()
      ),
      
      # Show a plot of the selected variables
      mainPanel(
         plotOutput("plot")
      )
   )
))

# Define server logic
server <- shinyServer(function(input, output) {
  shinyURL.server()
  
  # Return the requested dataset
  dataset <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
    })
  
  # Variables available in the selected dataset
  variables = reactive(names(dataset()))
  
  # Render select input for the X variable
  output$varxInput = renderUI({
    req(input$dataset)
    selectInput("varX", label = "X variable", choices = variables())
  })
  
  # Render select input for the Y variable
  output$varyInput = renderUI({
    req(input$varX %in% variables())
    selectInput("varY", label = "Y variable",
                choices = variables()[variables() != input$varX])
  })
  
  output$plot <- renderPlot({
    req(input$varY %in% variables())
    plot(x = dataset()[,input$varX], y = dataset()[,input$varY], 
         xlab = input$varX, ylab = input$varY)
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
