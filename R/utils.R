debugMsg = function(...) {
  if (isTRUE(getOption("shinyURL.debug"))) {
    cat(..., "\n")
    flush.console()
  }
  invisible()
}
