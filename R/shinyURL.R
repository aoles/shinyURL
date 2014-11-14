#' Encode current application state into URL
#' @param session parameter passed from the \code{shinyServer} function
#' @import shiny
#' @export 
encodeShinyURL = function(session) {
  observe({
  ## all.names = FALSE excludes objects with a leading dot, in this case the ".url" field to avoid self-dependency
  inputValues = reactiveValuesToList(session$input, all.names = FALSE)
  
  ## discard group inputs as their elements already have individual IDS
  inputValues = inputValues[sapply(inputValues, length) == 1]
  
  ## compress TRUE/FALSE to T/F
  inputValues = lapply(inputValues, function(x) {
    if ( is.logical(x) ) {
      if (isTRUE(x)) "T" else "F"
    } else x  
  })
  
  updateTextInput(session, ".url", value = paste0(
    session$clientData$url_protocol, "//",
    session$clientData$url_hostname,
    ## add port number if present
    if( (port=session$clientData$url_port)!="" ) paste0(":", port),
    session$clientData$url_pathname,        
    ## encode widget state
    "?", paste(names(inputValues), inputValues, sep = "=", collapse = "&") 
    ))
  }, priority = -100, suspended = TRUE)
}

#' Initialize application state from URL
#' @param session parameter passed from the \code{shinyServer} function
#' @param nestedDependency set to \code{TRUE} if you use reactive UI
#' @param self self-reference
#' @param encode reference to the URL encoder object
#' @param resume list of observers which start in a suspended state and need to be resumed after initialization finishes 
#' @export
initFromURL = function(session, nestedDependency = FALSE, self, encode, resume = list(encode)) {
  observe({
    # execute only once at startup
    self$suspend()
      
    query = parseQueryString(session$clientData$url_search)
    
    #initial pass needed due to nested dependency of concentration on drug 
    if ( isTRUE(nestedDependency) ) .initInputs(session, query)
    
    session$onFlushed( function() {
      .initInputs(session, query)
      for(i in resume) i$resume()
    })
  })
}

.initInputs = function(session, query) {
  for(i in seq_along(query))
    session$sendInputMessage(names(query)[i], list( value = ifelse(is.na((l = as.logical((q=query[[i]])))), q, l)) )
}
