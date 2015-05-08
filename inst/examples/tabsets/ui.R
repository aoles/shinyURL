library(shiny)
library(shinyURL)

shinyUI(fluidPage(
  titlePanel("Tabsets example"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Distribution type:",
                   c("Normal" = "norm",
                     "Uniform" = "unif",
                     "Exponential" = "exp")),
      br(),
      sliderInput("n", "Number of observations:", value = 500, min = 1, max = 1000),
      hr(),
      shinyURL.ui()
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", id = "tab",
        tabPanel("Plot", plotOutput("plot")), 
        tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
))
