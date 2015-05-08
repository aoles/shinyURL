library(shiny)
library(shinyURL)

shinyUI(fluidPage(
  
  h1("Shiny Widgets Gallery",
     style = "
      color: #fff;
      text-align: center;
      padding: 20px;
      background-color:#428bca;
      margin-top:0px;
      border-radius: 0px 0px 4px 4px;
     "),
  
  hr(),
  
  fluidRow(
    
    column(4, wellPanel(
      h3("Single checkbox"), 
      checkboxInput("checkbox", label = "Choice A", value = TRUE)
    )),
    
    column(4, wellPanel(
      checkboxGroupInput("checkGroup", 
      label = h3("Checkbox group"), 
      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
      selected = c(1,3))
    )),
    
    column(4, wellPanel(
      radioButtons("radio", 
      label = h3("Radio buttons"), 
      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
      selected = 1)
    ))
    
  ),
  
  fluidRow(
    
    column(4, wellPanel(
      dateInput("date", label = h3("Date input"))
    )),
    
    column(4, wellPanel(
      dateRangeInput("dates", label = h3("Date range"))
    )),
    
    column(4, wellPanel(
      textInput("text", label = h3("Text input"), value = "Enter text...")
    ))
    
  ),
  
  fluidRow(
    
    column(4, wellPanel(
      numericInput("num", label = h3("Numeric input"), value = 1)
    )),
    
    column(4, wellPanel(
      sliderInput("sliderA", label = h3("Slider"), min = 0, max = 100, value = 50)
    )),
    
    column(4, wellPanel(
      sliderInput("sliderB", label = h3("Slider range"), min = 0, max = 100, value = c(25, 75))
    ))
    
  ),
  
  fluidRow(
    
    column(4, wellPanel(
      selectInput("select", label = h3("Select box"), choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), selected = 1)
    )),
    
    column(8, wellPanel(
      shinyURL.ui(),
      style = "background-color:#428bca; color:#fff;"
    ))
    
  )
  
))
