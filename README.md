## Installation

    # install.packages("devtools")
    devtools::install_github("aoles/shinyURL")

## Use

1. Add to 'server.R' the following line

        library('shinyURL')
        
    and inside the `shinyServer` function a call to
    
        shinyURL(session)
        
    where `session` is the argument passed to the server function.
        

2. Add the text field containing the URL to 'ui.R'

        textInput(".url", "Share URL")

## Advice

The state of a shiny app gets saved by encoding its input values into an URL. To keep the URL compact and to avoid problems caused by the URL length limit (around 2000 characters) there are some points to keep in mind when developing your app.

1. Avoid long names of inputs but rather use short IDs. For example, instead of

        selectInput("firstDrug", "First drug", choices = drugs)
    
    it's better to have

        selectInput("d1", "First drug", choices = drugs)

2. Use named lists for the `choices` argument in `radioButtons` and `checkboxGroupInput`. Then only the names are displayed to the user allowing for shorter values of the control.

These points are especially relevant for apps with lots of controls.
