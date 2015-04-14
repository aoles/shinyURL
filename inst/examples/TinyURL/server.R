library(shiny)
library(shinyURL)
library(RCurl)

.busyMsg = "Please wait..."

shinyServer(function(input, output, session) {
  shinyURL(session)
  
  observeEvent( input$.getTinyURL, updateTextInput(session, ".url", value = .busyMsg), priority = 99 )
  
  tinyURLquery = eventReactive(input$.getTinyURL, sprintf("http://tinyurl.com/api-create.php?url=%s", input$.url), ignoreNULL = FALSE)
  
  observe({
    if ( is.null(tinyURLquery()) ) 
      return()
    if ( input$.url==.busyMsg ) {
      tinyurl = tryCatch(getURL(tinyURLquery()), error = function(e) "Error fetching tinyURL!")
      updateTextInput(session, ".url", value=tinyurl)
    }
  })
  
#   observe({
#     t = tinyURLquery()
#     tinyurl = tryCatch(getURL(t), error = function(e) "Error fetching tinyURL!")
#     updateTextInput(session, ".url", value=tinyurl)
#     })
      
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
})
  
