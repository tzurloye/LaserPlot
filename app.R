# Current required packages
require(shiny)
require(readxl)
require(readr)
require(ggplot2)
require(dplyr)
require(colourpicker)

# Load other functions
source("functions.R", local = TRUE)

# Global tags

#### User Interface (UI) ####
ui <- fluidPage(
  
  theme = "IUPUI.css",
  
  # This code is used to build what shows in the users browser tab when the app is loaded
  list(tags$head(HTML('<link rel="icon", href="trident_large.png",
                      type="image/png" />'))),
  div(style="padding: 0px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="LaserPlot v.2.2" # Change name and version here!
      )
  ),
  
  navbarPage(
    
    # The title section below is where you can alter the navbar title and logo
    title=div(img(src="trident_large.png", 
                  height = 40,
                  width = 40),
              "IUPUI Earth Sciences Apps"),
    
    # UI code for App Info tab
    tabPanel( "LaserPlot Info", fluid = TRUE,
              
               # this block of code supresses all error output in the final app
               # during development comment this out!!!!
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               ),
               # h3("This Page Intentionally Left Blank")

              includeHTML("Info/LaserPlotInfo.html")

    ),
    # close App Info tabPanel
    
    # UI code for Levitation Laser tab
    tabPanel( "Levitation Laser", fluid = TRUE,
               
               # # this block of code supresses all error output in the final app
               # # during development comment this out!!!!
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               ),
               
               source("uiLas.R", local = TRUE)$value
               
    )# close Levitation Laser tabPanel

  ) # close navbarPage
) # close fluidPage
#### End User Interface (UI) ####


##### Server ####
server <- function(input, output, session){
  
  # # stops the app when the browser session is closed
  # session$onSessionEnded(stopApp)
  
  # load Levitation Laser section server code
  source("serverLas.R", local = TRUE)$value
  
} 
#### End Server ####

# Initiate app by calling this function
shinyApp(ui=ui, server=server)
