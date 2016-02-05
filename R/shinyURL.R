#' Save and restore the view state of a Shiny app
#' 
#' Encode the state of Shiny app's widgets into an URL query string, and use 
#' parameters from the URL query string to initialize the app.
#' 
#' @section Quick setup: To start using shinyURL in your Shiny app, follow these
#'   three steps: \enumerate{ \item Load the package in both 'server.R' an 
#'   'ui.R': \code{library("shinyURL")} \item Add a call to \code{ 
#'   shinyURL.server()} inside the server function in 'server.R' \item Add the 
#'   \code{shinyURL.ui()} widget to 'ui.R'}
#' @author Andrzej Oleś <andrzej.oles@@embl.de>
#' @examples
#' if (interactive()) {
#'   library("shiny")
#'   
#'   ## A Simple Shiny App
#'    
#'   shinyApp(
#'     ui = fluidPage(
#'       titlePanel("Hello Shiny!"),
#'       sidebarLayout(
#'         sidebarPanel(
#'           sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
#'           shinyURL.ui()
#'         ),
#'         mainPanel(
#'           plotOutput("plot")
#'         )
#'       )
#'     ),
#'     server = function(input, output, session) {
#'       shinyURL.server(session)
#'       output$plot <- renderPlot({
#'         x <- faithful[, 2]
#'         bins <- seq(min(x), max(x), length.out = input$bins + 1)
#'         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#'       })
#'     }
#'   )
#' 
#'   ## Shiny Widgets Demo
#'   shinyAppDir( system.file('examples', 'widgets', package='shinyURL') )
#' 
#'   ## Tabsets Demo
#'   shinyAppDir( system.file('examples', 'tabsets', package='shinyURL') )
#'   
#'   ## Showcase demo available live at https://aoles.shinyapps.io/shinyURL
#'   shinyAppDir( system.file('examples', 'showcase', package='shinyURL') )
#' 
#' }
#' @name shinyURL
#' @importFrom shiny isolate observe parseQueryString observeEvent 
#'   updateTextInput eventReactive reactiveValuesToList invalidateLater 
#'   getDefaultReactiveDomain addResourcePath
#' @importFrom shiny tagList tags icon includeScript actionButton div 
#'   validateCssUnit
#' @importFrom RCurl getURL
#' @importFrom utils URLencode
NULL

inputId=".shinyURL"
