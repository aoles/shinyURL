#' Deprecated functions in package \sQuote{shinyURL}
#' 
#' These functions are provided for compatibility with older versions of
#' \sQuote{shinyURL} only, and will be defunct at the next release.
#' 
#' @details The following functions are deprecated and will be made defunct; use
#'   the replacement indicated below. \itemize{ \item{shinyURL.Server:
#'   \code{\link{shinyURL.server}}} \item{shinyURL.UI:
#'   \code{\link{shinyURL.ui}}} }
#' @param ... Arguments passed to the new methods
#' @rdname shinyURL-deprecated
#' @export
shinyURL.Server = function(...) {
  .Deprecated("shinyURL.server")
  shinyURL.server(...)
}

#' @rdname shinyURL-deprecated
#' @export
shinyURL.UI = function(...) {
  .Deprecated("shinyURL.ui")
  shinyURL.ui(...)
}

.invalidateOnInit = function(session, self) {
  observe({
    invalidateLater(0, session)
    self$suspend()
  })
}
