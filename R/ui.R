#' @details The \code{shinyURL.ui} widget consists of a text field containing an
#'   URL to the app's current view state.  By default it also features the 
#'   convenience \sQuote{Copy} button for copying the URL to clipboard, and a \sQuote{TinyURL}
#'   button for querying the URL shortening web service.  The inclusion of these
#'   buttons is optional and can be controlled by the \code{copyURL} and 
#'   \code{tinyURL} arguments, respectively.
#' @param label Label for the URL field
#' @param width The width of the URL text field, e.g. \code{'100\%'}, or 
#'   \code{'400px'}; see \code{\link[shiny]{validateCssUnit}}.
#' @param copyURL Include a \sQuote{Copy} button for convenient copying to clipboard
#' @param tinyURL Use the TinyURL web service for shortening the URL
#' @rdname shinyURL
#' @export
shinyURL.ui = function(label = "Share URL", width = "100%", copyURL = TRUE, tinyURL = TRUE) {
  div(
    class = "form-group shiny-input-container", # same as for textInput
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    
    ## URL text field
    if (!is.null(label) && !is.na(label)) tags$label(label, `for` = inputId),
    tags$input(id = inputId, type="text", class="form-control", value="", style="margin-bottom: 5px;"),
    
    ## Copy button
    if ( isTRUE(copyURL) )
      tagList(
        tags$button(id=".copyToClipboard", icon("clipboard"), "Copy", title="Copy to clipboard", type="button", class="btn btn-default", "data-clipboard-target"=inputId),
        includeScript(system.file("zeroclipboard", "ZeroClipboard.min.js", package="shinyURL"), type="text/javascript"),
        tags$script(
          type="text/javascript",
          "ZeroClipboard.config( { swfPath: '//cdnjs.cloudflare.com/ajax/libs/zeroclipboard/2.2.0/ZeroClipboard.swf' } );",
          "var client = new ZeroClipboard( document.getElementById('.copyToClipboard') );"
        )
      ),
    
    ## TinyURL button
    if ( isTRUE(tinyURL) )
      actionButton(".getTinyURL", "TinyURL", icon=icon("compress"), title="Shorten URL")
  )
}
