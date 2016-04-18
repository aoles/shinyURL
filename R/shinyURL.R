#' Save and restore the view state of a Shiny app
#' 
#' Encode the state of Shiny app's widgets into an URL query string, and use 
#' parameters from the URL query string to initialize the app.
#' 
#' @section Quick setup: To start using shinyURL in your Shiny app, follow these
#'   three steps: \enumerate{ \item Load the package in both 'server.R' an
#'   ui.R': \code{library("shinyURL")} \item Add a call to \code{ 
#'   shinyURL.server()} inside the server function \item Add the 
#'   \code{shinyURL.ui()} widget to the user interface}
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
#'   ## Showcase demo available live at https://gallery.shinyapps.io/shinyURL
#'   shinyAppDir( system.file('examples', 'showcase', package='shinyURL') )
#'   
#'   ## Interactive R Markdown document which uses a QR code to encode the URL
#'   if (require("rmarkdown") && require("qrcode"))
#'     run( system.file('examples', 'qrcode', 'qrcode.Rmd', package='shinyURL') )
#'     
#'   ## Use with dynamic user interface created by renderUI()
#'   shinyAppDir( system.file('examples', 'dynamicUI', package='shinyURL') )
#' }
#' @name shinyURL
#' @importFrom methods as
#' @importFrom shiny isolate observe parseQueryString observeEvent 
#'   updateTextInput eventReactive reactiveValuesToList invalidateLater 
#'   getDefaultReactiveDomain addResourcePath reactive
#' @importFrom shiny tagList tags icon includeScript actionButton div 
#'   validateCssUnit
#' @importFrom stats setNames
#' @importFrom RCurl getURL
#' @importFrom utils flush.console URLencode
NULL

inputId=".shinyURL"
