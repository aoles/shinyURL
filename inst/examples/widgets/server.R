library(shiny)
library(shinyURL)

shinyServer(function(input, output, session) {
  shinyURL.server(session)
})
