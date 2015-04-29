#' Save and restore the state of Shiny app's widgets
#' 
#' Encode the state of Shiny app's widgets into an URL query string, and use parameters from the URL query string to initialize the applications.
#' 
#' @section Quick setup:
#' To start using shinyURL in your Shiny app, follow these three steps:
#' \enumerate{
#'   \item Load the package in both 'server.R' an 'ui.R': \code{library("shinyURL")}
#'   \item Add a call to \code{ shinyURL.Server(session)} inside the 'shinyServer' function in 'server.R',
#'   where `session` is the argument passed to the server function.
#'   \item Add the \code{shinyURL.UI()} widget to 'ui.R'.
#' }
#' @author Andrzej Oleś <andrzej.oles@@embl.de>
#' @examples
#' if (interactive()) {
#'   shinyApp(
#'     ui = fluidPage(
#'       titlePanel("Hello Shiny!"),
#'       sidebarLayout(
#'         sidebarPanel(
#'           sliderInput("bins", "Number of bins:", min = 1,  max = 50, value = 30),
#'           shinyURL.UI()
#'         ),
#'         mainPanel(
#'           plotOutput("plot")
#'         )
#'       )
#'     ),
#'     server = function(input, output, session) {
#'       shinyURL.Server(session)
#'       output$plot <- renderPlot({
#'         x <- faithful[, 2]
#'         bins <- seq(min(x), max(x), length.out = input$bins + 1)
#'         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#'       })
#'     }
#'   )
#' }
#' 
#' @name shinyURL
#' @importFrom shiny isolate observe parseQueryString observeEvent updateTextInput eventReactive reactiveValuesToList invalidateLater
#' @importFrom shiny tagList textInput tags icon includeScript actionButton
#' @importFrom RCurl getURL
#' @importFrom utils URLencode
NULL

inputId=".shinyURL"

#' @details The \code{shinyURL.Server} method contains server logic for encoding
#'   and restoring the widgets' state. It is called from inside the app's server
#'   function with the \code{session} argument.
#' @param session Parameter passed from the Shiny server function
#' @rdname shinyURL
#' @export
shinyURL.Server = function(session) {
  session$queryValues <- isolate(parseQueryString(session$clientData$url_search, nested=TRUE))
  
  ## initialize from query string
  init = .initFromURL(session, init)
  ## encode current app's state
  .encodeURL(session, inputId)
  
  ## use TinyURL for shortening URL
  useTinyURL = ".getTinyURL" %in% names(isolate(reactiveValuesToList(session$input, all.names=TRUE)))
  
  if ( useTinyURL ) {
    input = session$input
    .busyMsg = "Please wait..."
    
    observeEvent( input$.getTinyURL, updateTextInput(session, inputId, value=.busyMsg) )
    
    tinyURLquery = eventReactive(input$.getTinyURL, sprintf("http://tinyurl.com/api-create.php?url=%s", input[[inputId]]) )
    
    observe({
      if ( is.null(tinyURLquery()) ) 
        return()
      if ( input[[inputId]]==.busyMsg ) {
        tinyurl = tryCatch(getURL(tinyURLquery()), error = function(e) "Error fetching tinyURL!")
        updateTextInput(session, inputId, value=tinyurl)
      }
    })
  }
  
  ## Initial invalidation needed to execute scheduled input updates when the browser is refreshed
  invalidate = .invalidateOnInit(session, invalidate)
    
  invisible(NULL)
}

.invalidateOnInit = function(session, self) {
  observe({
    cat("Invalidate\n")
    invalidateLater(0, session)
    self$suspend()
  })
}

#' @details The \code{shinyURL.UI} inserts a text field widget containing an URL to
#'   the app's current view.  By default it also features the convenience 'Copy'
#'   button copying the URL to clipboard, and a 'TinyURL' button querying the 
#'   URL shortening web service.  The inclusion of these buttons is optional and
#'   can be controlled by the \code{copyURL} and \code{tinyURL} arguments, 
#'   respectively.
#' @param label Label for the URL field
#' @param copyURL Include a 'Copy' button for convenient copying to clipboard
#' @param tinyURL Use the TinyURL web service for shortening the URL
#' @rdname shinyURL
#' @export
shinyURL.UI = function(label="Share URL", copyURL=TRUE, tinyURL=TRUE) tagList(
  textInput(inputId, label),
  if ( isTRUE(copyURL) )
    tagList(
      tags$button(id=".copy", icon("clipboard"), "Copy", title="Copy to clipboard", type="button", class="btn btn-default", "data-clipboard-target"=inputId),
      includeScript(system.file("zeroclipboard", "ZeroClipboard.min.js", package="shinyURL")),
      tags$script(type="text/javascript",
                  paste(collapse="\n", c("",
                                         "ZeroClipboard.config( { swfPath: 'http://cdnjs.cloudflare.com/ajax/libs/zeroclipboard/2.2.0/ZeroClipboard.swf' } );",
                                         "var client = new ZeroClipboard( document.getElementById('.copy') );",
                                         ""))
      )
    ),
  if ( isTRUE(tinyURL) )
    actionButton(".getTinyURL", "TinyURL", icon=icon("compress"), title="Shorten URL")
)

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
    else {
      q = unname(q)
    }
    session$sendInputMessage(names(query)[i], list(value=q))
  }
}
