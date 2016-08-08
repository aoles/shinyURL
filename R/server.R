#' @details The \code{shinyURL.server} method contains server logic for encoding
#'   and restoring the widgets' values. It is called from inside the app's 
#'   server script, and can take the \code{session} objects as argument.
#'   
#'   The argument \code{options} can contain a named list of options. These are
#'   set by a call to \code{\link[base]{options}} as \sQuote{shinyURL.name}. See below for a list of available options.
#' @section ShinyURL options:  
#' \describe{
#'  \item{\code{debug = TRUE}}{Print debug messages to the console}
#' }
#' @param session Typically the same as the optional parameter passed into the 
#'   Shiny server function as an argument; if missing defaults to 
#'   \code{getDefaultReactiveDomain()}
#' @param options Named list of options
#' @return \code{shinyURL.server} returns a reactive expression evaluating to 
#'   the app's URL.
#' @rdname shinyURL
#' @export
shinyURL.server = function(session, options) {
  if (missing(session))
    session = getDefaultReactiveDomain()
  
  if (!missing(options))
    options(setNames(options, paste("shinyURL", names(options), sep=".")))
  
  debugMsg("ShinyURL initializes")
  
  ## initialize from query string
  init = .initFromURL(session, init)
  
  ## encode current app's state
  url = .encodeURL(session, inputId)
  
  ## use TinyURL for shortening the URL
  .queryTinyURL(session)
  
  ## Initial invalidation needed to execute scheduled input updates when the 
  ## browser is refreshed switched off because it interferes with dynamic UIs 
  ## invalidate = .invalidateOnInit(session, invalidate)
  
  url
}


.initFromURL = function(session, self) {
  queryValues <- isolate(parseQueryString(session$clientData$url_search, nested=TRUE))
  observe({
    debugMsg(".initFromURL")
    queryValuesCopy = queryValues
    
    ## iterate through available inputs as long as there are any uninitialized 
    ## values in queryValues the expression below depends on inputs which is 
    ## neccassary to restore dynamic UIs
    inputValues = reactiveValuesToList(session$input, all.names=FALSE)
    updateValues = intersect(names(inputValues), names(queryValues))
    queryIds = match(updateValues, names(queryValues))
    inputIds = match(updateValues, names(inputValues))
    
    if ( length(queryIds) > 0 ) queryValues <<- queryValues[-queryIds]
    
    ## schedule the update only after all input messages have been sent out (see
    ## the 'flushOutput' function in shiny.R). This is to avoid potential 
    ## overwriting by some update events from user code
    #session$onFlushed(function() {
    .initInputs(session, queryValuesCopy[queryIds], inputValues[inputIds])
    #})
    
    ## suspend if nothing to do
    if ( length(queryValues) == 0L )
      self$suspend()
    
  }, priority = -99)
}


.initInputs = function(session, queryValues, inputValues) {
  
  for (i in seq_along(queryValues)) {
    q = queryValues[[i]]
    
    q = if (is.list(q)) {
      ## checkbox group or multiple select
      unlist(q, use.names=FALSE) 
    }
    else {
      ## decode range vectors (sliders and dates)
      if (length(inputValues[[i]])>1L)
        q = unlist(strsplit(q, ","))
      ## use information about the class of the inputs when initializing them
      cl = class(inputValues[[i]])
      ## promote integer to numeric because numericInputs can contain either
      if (cl=="integer")
        cl = "numeric" 
      switch(cl,
             ## selectInput without default value is initially set to NULL
             NULL = q,
             ## Dates need to be handled separately
             Date = format(as.Date(as.numeric(q), "1970-01-01"), "%Y-%m-%d"),
             ## default case; should allow to correctly decode TRUE/FALSE
             as(q, cl)
      )
    }
    debugMsg("init", names(queryValues)[i], "->", q)
    session$sendInputMessage(names(queryValues)[i], list(value=q))
  }
}


.encodeURL = function(session, inputId) {
  clientData = isolate(reactiveValuesToList(session$clientData))
  
  ## base URL which is not supposed to change
  baseURL = paste0(clientData$url_protocol, "//",
                   clientData$url_hostname,
                   ## add port number if present
                   if( (port=clientData$url_port)!="" ) paste0(":", port),
                   clientData$url_pathname)
  
  queryString = reactive({
    ## all.names = FALSE excludes objects with a leading dot, in particular the
    ## ".url" field to avoid self-dependency
    inputValues = reactiveValuesToList(session$input, all.names=FALSE)
    
    ## quit if there is there are no inputs to encode
    if (length(inputValues)==0) return()
    
    ## remove actionButtons
    isActionButton = unlist(lapply(inputValues, inherits, "shinyActionButtonValue"), use.names=FALSE)
    inputValues = inputValues[!isActionButton]
    
    ## remove ggvis specific inputs
    idx = grep("_mouse_(over|out)$", names(inputValues))
    if ( length(idx) > 0 ) inputValues = inputValues[-idx]
    
    inputValues = mapply(function(name, value) {
      ## this is important to be able to have all checkboxes unchecked
      if (is.null(value))
        ""
      else {
        if (length(value) == 1L) {
          ## encode TRUE/FALSE as T/F
          if (is.logical(value)) {
            if (isTRUE(value)) "T" else "F"
          }
          else value
        }
        else {
          cl = class(value)
          ## expand checkbox group and multiple select vectors
          if (cl=="character") {
            setNames(as.list(value), sprintf("%s[%s]", name, seq_along(value)))
          }
          ## encode range vectors as comma separated string
          else {
            if (cl=="Date") value = as.integer(value)
            paste(value, collapse=",")
          } 
        }
      }
    }, names(inputValues), inputValues, SIMPLIFY=FALSE)
    
    ## remove names of sublists before flattening
    names(inputValues)[sapply(inputValues, is.list)] = ""
    inputValues = unlist(inputValues)
    
    URLencode(paste(names(inputValues), inputValues, sep = "=", collapse = "&"))
  })
  
  observe({
    debugMsg(".updateURL")
    updateTextInput(session, inputId, value = url())
    updateTextInput(session, ".shinyURL.queryString", value = queryString())
  }, priority = -999)
  
  url = reactive({
    paste(c(baseURL, queryString()), collapse = "?")
  })
  
  url
}


.queryTinyURL = function(session) {
  input = session$input
  .busyMsg = "Please wait..."
  
  ## construct a query string from the current URL
  tinyURLquery = eventReactive(input$.getTinyURL, {
    sprintf("http://tinyurl.com/api-create.php?url=%s", input[[inputId]]) 
  })
  
  ## set busy message
  observeEvent(tinyURLquery(), {
    updateTextInput(session, inputId, value=.busyMsg)
    
    ## resume the observer only after .busyMsg is set
    session$onFlushed(function() {
      runTinyURLquery$resume()
    })
  })
  
  ## query TinyURL
  runTinyURLquery = observe({
    tinyurl = tryCatch(getURL(tinyURLquery()), error = function(e) "Error fetching tinyURL!")
    updateTextInput(session, inputId, value=tinyurl)
    runTinyURLquery$suspend()
  }, suspended=TRUE)
  
  invisible()
}
