## Installation

    # install.packages("devtools")
    devtools::install_github("aoles/shinyURL")

## Use

1. Add to 'server.R' and 'ui.R'
    
        library('shinyURL')

2. Add the textfield for the URL to 'ui.R'

        textInput(".url", "", value = "")

3. To 'server.R' add the following

        init = initFromURL(session, self = init, encode = encode)
        
        encode = encodeShinyURL(session, input)
        
    Note that you need to provide a self reference and a reference to the 'encodeShinyURL' object in the call to 'initFromURL'.
