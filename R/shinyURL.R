#' Save and restore the state of a Shiny App
#' 
#' Encode the state of a Shiny application into an URL and use URL parameters to initialize the app.
#' @param session parameter passed from the \code{shinyServer} function
#' @param nestedDependency set to \code{TRUE} if you use reactive UI
#' @param resume list of observers which start in a suspended state and need to be resumed after initialization completes
#' @param inputId id of the textfield containing the encoded URL
#' @import shiny
#' @export 
shinyURL = function(session, nestedDependency = FALSE, resume = NULL, inputId = ".url") {
  encode = .encodeURL(session, inputId)
  init = .initFromURL(session, nestedDependency, init, encode, c(encode, resume))
}

#' Encode current application state into URL
#' @param session parameter passed from the \code{shinyServer} function
#' @param input parameter passed from the \code{shinyServer} function
#' @note This function has been deprecated. Please use \code{\link{shinyURL}} instead.
#' @export
encodeShinyURL = function(session, input) {
  .Deprecated("shinyURL")
  .encodeURL(session)
}
  
.encodeURL = function(session, inputId) {
  observe({
  ## all.names = FALSE excludes objects with a leading dot, in this case the ".url" field to avoid self-dependency
  inputValues = reactiveValuesToList(session$input, all.names = FALSE)
  
  ## discard individual elements of CheckboxGroups and expand CheckboxGroup vectors
  names = names(inputValues)
  i = 0
  addValues = NULL
  while (i < length(names) ) {        
    i = i+1
    n = names[i]
    idx = grep(sprintf("^%s[0-9]+", n), names)
    
    ## encountered CheckboxGroup
    if ( length(idx)>0 ) {
      value = inputValues[[n]]
      names = names[-c(i, idx)]
      if ( !is.null(value) ) {
        values = as.list(value)
        names(values) = sprintf("%s[%s]", n, seq_along(value))
        addValues = c(addValues, values)
      }
    }
  }
  inputValues = inputValues[names]
  inputValues = c(inputValues, addValues)
  
  ## compress TRUE/FALSE to T/F
  inputValues = lapply(inputValues, function(x) {
    if ( is.logical(x) ) {
      if (isTRUE(x)) "T" else "F"
    } else x  
  })
  
  updateTextInput(session, inputId, value = paste0(
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
#' @note This function has been deprecated. Please use \code{\link{shinyURL}} instead.
#' @export
initFromURL = function(session, nestedDependency = FALSE, self, encode, resume = NULL) {
  .Deprecated("shinyURL")
  .initFromURL(session, nestedDependency, self, encode, resume)
}

.initFromURL = function(session, nestedDependency = FALSE, self, encode, resume = NULL) {
  #.Deprecated("shinyURL$new()", msg = "The interface of shinyURL has been overhauled.")
  observe({
    # execute only once at startup
    self$suspend()
      
    query = parseQueryString(session$clientData$url_search, nested=TRUE)
    
    #initial pass needed due to nested dependency of concentration on drug 
    #if ( isTRUE(nestedDependency) ) .initInputs(session, query)
    
    .initInputs(session, query)
    session$onFlushed( function() {
      for(i in resume) i$resume()
    })
  }, priority = 100)
}

.initInputs = function(session, query) {
  for(i in seq_along(query))
    session$sendInputMessage(names(query)[i], list( value = ifelse(is.na((l = as.logical((q=query[[i]])))), q, l)) )
}
