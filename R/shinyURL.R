#' Save and restore the state of Shiny app's widgets
#' 
#' Encode the state of Shiny app's widgets into an URL query string, and use
#' parameters from the URL query string to initialize the applications.
#' 
#' @section Quick setup: To start using shinyURL in your Shiny app, follow these
#'   three steps: \enumerate{ \item Load the package in both 'server.R' an
#'   'ui.R': \code{library("shinyURL")} \item Add a call to \code{
#'   shinyURL.server(session)} inside the 'shinyServer' function in 'server.R', 
#'   where `session` is the argument passed to the server function. \item Add
#'   the \code{shinyURL.ui()} widget to 'ui.R'. }
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
#'           sliderInput("bins", "Number of bins:", min = 1,  max = 50, value = 30),
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
#' } 
#' @name shinyURL
#' @importFrom shiny isolate observe parseQueryString observeEvent
#'   updateTextInput eventReactive reactiveValuesToList invalidateLater
#' @importFrom shiny tagList tags icon includeScript actionButton div validateCssUnit
#' @importFrom RCurl getURL
#' @importFrom utils URLencode
NULL

inputId=".shinyURL"

#' @details The \code{shinyURL.server} method contains server logic for encoding
#'   and restoring the widgets' state. It is called from inside the app's server
#'   function with the \code{session} argument.
#' @param session Parameter passed from the Shiny server function
#' @rdname shinyURL
#' @export
shinyURL.server = function(session) {
  queryValues <- isolate(parseQueryString(session$clientData$url_search, nested=TRUE))
  
  ## initialize from query string
  init = .initFromURL(session, queryValues, init)
  
  ## encode current app's state
  .encodeURL(session, inputId)
  
  ## use TinyURL for shortening the URL
  .queryTinyURL(session)
  
  ## Initial invalidation needed to execute scheduled input updates when the
  ## browser is refreshed switched off because it interferes with dynamic UIs 
  ## invalidate = .invalidateOnInit(session, invalidate)
    
  invisible(NULL)
}


#' Deprecated functions
#' 
#' @param ... Arguments passed to the new methods \code{\link{shinyURL.server}}
#'   and \code{\link{shinyURL.ui}}.
#' @rdname deprecated
#' @export
shinyURL.Server = function(...) {
  .Deprecated("shinyURL.server")
  shinyURL.server(...)
}

.invalidateOnInit = function(session, self) {
  observe({
    invalidateLater(0, session)
    self$suspend()
  })
}

#' @details The \code{shinyURL.ui} inserts a text field widget containing an URL
#'   to the app's current view.  By default it also features the convenience
#'   'Copy' button copying the URL to clipboard, and a 'TinyURL' button querying
#'   the URL shortening web service.  The inclusion of these buttons is optional
#'   and can be controlled by the \code{copyURL} and \code{tinyURL} arguments, 
#'   respectively.
#' @param label Label for the URL field
#' @param width The width of the URL text field, e.g. \code{'100\%'}, or
#'   \code{'400px'}; see \code{\link[shiny]{validateCssUnit}}.
#' @param copyURL Include a 'Copy' button for convenient copying to clipboard
#' @param tinyURL Use the TinyURL web service for shortening the URL
#' @rdname shinyURL
#' @export
shinyURL.ui = function(label = "Share URL", width = "100%", copyURL = TRUE, tinyURL = TRUE)
  div(
    class = "form-group shiny-input-container", # same as for textInput
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    
    ## URL text field
    if (!is.null(label) && !is.na(label)) tags$label(label, `for` = inputId),
    tags$input(id = inputId, type="text", class="form-control", value="", style="margin-bottom: 5px;"),
    
    ## Copy button
    if ( isTRUE(copyURL) )
      tagList(
        tags$button(id=".copy", icon("clipboard"), "Copy", title="Copy to clipboard", type="button", class="btn btn-default", "data-clipboard-target"=inputId),
        includeScript(system.file("zeroclipboard", "ZeroClipboard.min.js", package="shinyURL"), type="text/javascript"),
        tags$script(
          type="text/javascript",
          "ZeroClipboard.config( { swfPath: '//cdnjs.cloudflare.com/ajax/libs/zeroclipboard/2.2.0/ZeroClipboard.swf' } );",
          "var client = new ZeroClipboard( document.getElementById('.copy') );"
        )
      ),
    
    ## TinyURL button
    if ( isTRUE(tinyURL) )
      actionButton(".getTinyURL", "TinyURL", icon=icon("compress"), title="Shorten URL")
  )


#' @rdname deprecated
#' @export
shinyURL.UI = function(...) {
  .Deprecated("shinyURL.ui")
  shinyURL.ui(...)
}

initFromURL = function(session, nestedDependency = FALSE, self, encode, resume = NULL) {
  .Defunct("shinyURL.server")
}

.encodeURL = function(session, inputId) {
  observe({
    ## all.names = FALSE excludes objects with a leading dot, in this case the
    ## ".url" field to avoid self-dependency
    inputValues = reactiveValuesToList(session$input, all.names=FALSE)
    
    ## quit if there is there are no inputs to encode
    if (length(inputValues)==0) return()
    
    ## remove actionButtons
    isActionButton = unlist(lapply(inputValues, function(x) inherits(x, "shinyActionButtonValue")), use.names=FALSE)
    inputValues = inputValues[!isActionButton]
    
    ## remove ggvis specific inputs
    idx = grep("_mouse_(over|out)$", names(inputValues))
    if ( length(idx) > 0 ) inputValues = inputValues[-idx]
    
    ## NOTE: the default values of checkbox groups are encoded as TRUE/FALSE 
    ## values of individual elements named as checkboxGroupNameX, where X is the
    ## element index; the _values_ set by user are stored as a vector under the 
    ## checkboxGroupName
    
    ## NOTE2: The behavior described above has changed in shiny 0.12.0, so that 
    ## the default values are no longer stored as checkboxGroupNameX elements. 
    ## Therefore, the code below can be simplified if dependency on shiny (>= 
    ## 0.12.0) is stated in the DESCRIPTION
    
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
        ## encountered CheckboxGroup; do not increment i because the
        ## corresponding element is removed from the list
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
    
    inputValues = lapply(inputValues, function(x) {
      if (length(x) == 1L) {
        ## encode TRUE/FALSE as T/F
        if ( is.logical(x) ) {
          if (isTRUE(x)) "T" else "F"
        }
        else x
      }
      else {
        ## encode vectors as comma separated lists
        if (class(x)=="Date") x = as.integer(x)
        paste(x, collapse=",")
      }
    })
    
    url = URLencode(paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname,
      ## add port number if present
      if( (port=session$clientData$url_port)!="" ) paste0(":", port),
      session$clientData$url_pathname,
      ## encode widget state
      "?", paste(names(inputValues), inputValues, sep = "=", collapse = "&")
    ))
    
    updateTextInput(session, inputId, value = url)
  }, priority = -999)
}

.initFromURL = function(session, queryValues, self) {
  observe({
    queryValuesCopy = queryValues
    
    ## suspend if nothing to do
    if ( length(queryValues) == 0L ) {
      self$suspend()
      return(NULL)
    }
    
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
    session$onFlushed(function() {
      .initInputs(session, queryValuesCopy[queryIds], inputValues[inputIds])
    })
    
    invisible(NULL)
  }, priority = -99)
}

.initInputs = function(session, queryValues, inputValues) {
  for (i in seq_along(queryValues)) {
    q = queryValues[[i]]
    
    q = if (is.list(q)) {
      ## checkbox group
      unname(q)
    }
    else {
      ## decode vectors (ranges sliders, date ranges)
      if (length(inputValues[[i]])>1L)
        q = unlist(strsplit(q, ","))
      ## use information about the class of the inputs when initializing them
      cl = class(inputValues[[i]])[1L]
      switch(cl,
             ## Dates need to be handled separately
             Date = format(as.Date(as.numeric(q), "1970-01-01"), "%Y-%m-%d"),
             ## default case; should allow to correctly decode TRUE/FALSE
             as(q, cl)
             )
    }
    session$sendInputMessage(names(queryValues)[i], list(value=q))
  }
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
    runTinyURLquery$resume()
  })
  
  ## query TinyURL
  runTinyURLquery = observe({
    if ( input[[inputId]]==.busyMsg ) {
      tinyurl = tryCatch(getURL(tinyURLquery()), error = function(e) "Error fetching tinyURL!")
      updateTextInput(session, inputId, value=tinyurl)
      runTinyURLquery$suspend()
    }
  }, suspended=TRUE)
}
