## Installation

    # install.packages("devtools")
    devtools::install_github("aoles/shinyURL")

## Use

1. Add to 'server.R'

        library('shinyURL')
        
    and inside the `shinyServer` function the following line
    
        shinyURL(session)
        
    where `session` is the argument passed to the server function.
        

2. Add the textfield containing the URL to 'ui.R'

        textInput(".url")        
