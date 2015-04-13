#' Save and restore the state of a Shiny App
#' 
#' Encode the state of a Shiny application into an URL and use URL parameters to initialize the app.
#' @param session parameter passed from the \code{shinyServer} function
#' @param inputId id of the text field containing the encoded URL
#' @import shiny
#' @export
shinyURL = function(session, inputId=".url") {
  session$queryValues <- isolate(parseQueryString(session$clientData$url_search, nested=TRUE))
  
  ## initialize from query string
  init = .initFromURL(session, init)
  ## encode current app's state
  .encodeURL(session, inputId)
  
  invisible(NULL)
}

#' Encode current application state into URL
#' @param session parameter passed from the \code{shinyServer} function
#' @note This function has been deprecated. Please use \code{\link{shinyURL}} instead.
#' @export
encodeShinyURL = function(session) {
  .Deprecated("shinyURL")
  .encodeURL(session)
}

.encodeURL = function(session, inputId) {
  observe({
    ## all.names = FALSE excludes objects with a leading dot, in this case the ".url" field to avoid self-dependency
    inputValues = reactiveValuesToList(session$input, all.names=FALSE)
    ## remove ggvis specific inputs
    idx = grep("_mouse_(over|out)$", names(inputValues))
    if ( length(idx) > 0 ) inputValues = inputValues[-idx]
    
    ## NOTE: the default values of checkbox groups are encoded as TRUE/FALSE
    ## values of individual elements named as checkboxGroupNameX, where X is the
    ## element index; the _values_ set by user are stored as a vector under the
    ## checkboxGroupName
    
    ## sort to catch checkboxGroupNames before individual element names
    names = sort(names(inputValues))
    addValues = NULL
    i = 1L
    while (i <= length(names) ) {
      n = names[i]
      idx = grep(sprintf("^%s[0-9]+", n), names)
      
      if ( length(idx) == 0L ) {
        ## advance to the next input
        i = i + 1L
      } else {
        ## encountered CheckboxGroup; do not increment i because the corresponding
        ## element is removed from the list
        value = inputValues[[n]]
        names = names[-c(i, idx)]
        ## this is important to be able to have all checkboxes unchecked
        if ( is.null(value) ) value = ""
        values = as.list(value)
        ## expand CheckboxGroup vectors
        names(values) = sprintf("%s[%s]", n, seq_along(value))
        addValues = c(values, addValues)
      }
    }
    inputValues = inputValues[names]
    inputValues = c(inputValues, addValues)
    
    ## encode TRUE/FALSE as T/F
    inputValues = lapply(inputValues, function(x) {
      if ( is.logical(x) ) {
        if (isTRUE(x)) "T" else "F"
      } else x
    })
    
    url = paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname,
      ## add port number if present
      if( (port=session$clientData$url_port)!="" ) paste0(":", port),
      session$clientData$url_pathname,
      ## encode widget state
      "?", paste(names(inputValues), inputValues, sep = "=", collapse = "&")
    )
    
    updateTextInput(session, inputId, value = url)
  }, priority = -999)
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
  .initFromURL(session, self)
}

.initFromURL = function(session, self) {
  observe({
    queryValues = session$queryValues
    ## suspend if nothing to do
    if ( length(queryValues) == 0L ) {
      self$suspend()
      return(NULL)
    }
    
    ## iterate through available inputs as long as there are any uninitialized values in queryValues
    ## the expression below depends on inputs which is neccassary to restore dynamic UIs
    inputValues = reactiveValuesToList(session$input, all.names=FALSE)
    
    updateValues = intersect(names(inputValues), names(queryValues))
    idx = match(updateValues, names(queryValues))
    updateValues = queryValues[idx]
    
    if ( length(idx) > 0 ) session$queryValues = queryValues[-idx]
    
    ## schedule the update only after all input messages have been sent out (see
    ## the 'flushOutput' function in shiny.R). This is to avoid potential
    ## overwriting by some update events from user code
    session$onFlushed(function() {
      .initInputs(session, updateValues)
    })
    
    invisible(NULL)
  }, priority = -99)
}

.initInputs = function(session, query) {
  for (i in seq_along(query)) {
    q = query[[i]]
    if (!is.list(q)) {
      ## parse numeric vectors (needed by range sliders)
      if ( isTRUE(grepl("^c\\( *([[:digit:]]+, *)*[[:digit:]]+ *\\)$", q)) )
        q = eval(parse(text=q))
      ## decode TRUE/FALSE
      else if ( isTRUE(grepl("^[TF]$", q)) )
        q = as.logical(q)
    }
    session$sendInputMessage(names(query)[i], list(value=q))
  }
}
