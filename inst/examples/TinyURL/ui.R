library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      ## shinyURL
      textInput(".url", "Share URL"),
      tags$button(id=".copy", icon("clipboard"), "Copy", title="Copy to clipboard", class="btn btn-default", "data-clipboard-target"=".url"),
      actionButton(".getTinyURL", "TinyURL", icon=icon("compress"), title="Shorten URL", "data-clipboard-target"=".url"),
      tagList(
        includeScript(system.file("zeroclipboard", "ZeroClipboard.min.js", package="shinyURL")),
        tags$script(type="text/javascript",
          paste(collapse="\n", c("",
            "ZeroClipboard.config( { swfPath: 'http://cdnjs.cloudflare.com/ajax/libs/zeroclipboard/2.2.0/ZeroClipboard.swf' } );",
            "var client = new ZeroClipboard( document.getElementById('.copy') );",
            ""))
        )
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
