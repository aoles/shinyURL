## Installation

    # install.packages("devtools")
    devtools::install_github("aoles/shinyURL")

## Use

1. Load the package in both 'server.R' and 'ui.R':

        library("shinyURL")

2. In 'server.R', add a call to
    
        shinyURL.Server(session)
        
    inside the `shinyServer` function, where `session` is the argument passed to the server function.
        
3. Add the shinyURL widget to 'ui.R':

        shinyURL.UI()

## Limitations

### Long URLs

The state of a shiny app gets saved by encoding its input values into an URL. To keep the URL compact and to avoid problems caused by the URL length limit (around 2000 characters) there are some points to keep in mind when developing your app.

1. Avoid long names of inputs but rather use short IDs. For example, instead of

        selectInput("firstDrug", "First drug", choices = drugs)
    
    it's better to have

        selectInput("d1", "First drug", choices = drugs)

2. Use named lists for the `choices` argument in `radioButtons` and `checkboxGroupInput`. Then only the names are displayed to the user allowing for shorter values of the control.

These points are especially relevant for apps with lots of controls.

### Input IDs

Avoid using input IDs which differ by appended numbers, i.e. do not use `value` along with `value2`.

### Action buttons

Unfortunately, operations performed using action buttons cannot be reliably restored. You can omit them from the query URL by using IDs with a leading dot, e.g. `.myButton`.
