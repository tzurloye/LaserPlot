tabsetPanel(

  # Data Tab       
  tabPanel("Data",
           
           # Data Sidebar Panel
           sidebarPanel( width = 3,
                         
                         h5("Download Sample Levitation Laser Output"),
                         downloadButton('sampleDataDownload', "Sample Data File"),
                         
                         h3( "Load Your Data" ), 
                         
                         # input selector for choosing data file
                         fileInput( "LasFile",
                                    "Select Excel data file",
                                    accept = c(".xls, .xlsx")),
                         
                         # input selector for X variable
                         uiOutput( "LasChoose_A" ),
                         
                         # input selector for Y variable
                         uiOutput( "LasChoose_B" ),
                         
                         # input selector for Y2 variable
                         uiOutput( "LasChoose_C" )
                         
           ),
           
           # Data Tab Main Panel
           mainPanel(
             tabsetPanel(
               tabPanel("Input Data",
                        dataTableOutput("LasFullData")
               ),
               tabPanel("Levitation Laser Data",
                        dataTableOutput("LasPlotData")
               )
             )
           )
           
  ),# Close Data Tab
  
  # Plot Tab       
  tabPanel("Plot",
           fluidPage(
             
             # Plot Left Panel
             column(width = 3,
                    
                    # Panel for point colors and shapes
                    h4("Plot Controls"),
                    wellPanel(
                      h5("Color Controls"),
                      colourInput("LasOnCol", "Laser On", "#FF0000", allowTransparent = TRUE),
                      colourInput("LasOffCol", "Laser Off", "#0000FF", allowTransparent = TRUE),
                      colourInput("LasPowerCol", "Laser Power", "#8A8A8A", allowTransparent = TRUE),
                      br(),
                      h5("Additional Controls"),
                      checkboxInput("LasAddCoolReg", "Add cooling rate regression", value = TRUE),
                      checkboxInput("LasAddHeatReg", "Add heating rate regression", value = TRUE),
                      numericInput("TempPlotThickness", "Select line thickness for Temperature vs Time plot:", value = 1.5, step = 0.5),
                      numericInput("PowerPlotThickness", "Select line thickness for Laser Power vs Time plot:", value = 1.5, step = 0.5)
                    ),
                    wellPanel(
                      h4("Sample Gas Flow (CC/minute):"),
                      textOutput("LasGasFlow"),
                      textInput("LasPlotGas", "Input Gas Flow if it Changed After Turning the Laser On:", value = NULL),
                      actionButton("question", "*Info*"),
                      textInput("LasTableNotes", "Additional Notes:", value = NULL)  
                    )
             ),
             
             # Plot Center Panel
             column( width = 6,
                     plotOutput("LasPlotTemp",
                                width = '100%',
                                height = 540,
                                hover = "LasPlotTemp_hover",
                                brush = brushOpts(id = "LasPlotTemp_brush", resetOnNew = TRUE),
                                dblclick = "LasPlotTempDblClick"),
                     verbatimTextOutput("infoTemp"),
                     hr(),
                     plotOutput("LasPlotPower",
                                width = '100%',
                                height = 540,
                                hover = "LasPlotPower_hover",
                                brush = brushOpts(id = "LasPlotPower_brush", resetOnNew = TRUE),
                                dblclick = "LasPlotPowerDblClick"),
                     verbatimTextOutput("infoPower")
             ),
             
             # Plot right panel
             column( width = 3,
                     h4("Download Options:"),
                     fluidRow(
                       column(9, 
                              downloadButton('LasDownloadTPlot', "Download Temp vs Time Plot"),
                              tags$style(type='text/css', "#LasDownloadTPlot { margin-top: 25px}"),
                              downloadButton('LasDownloadPPlot', "Download Power vs Time Plot"),
                              tags$style(type='text/css', "#LasDownloadPPlot { margin-top: 25px}"),
                              downloadButton('LasDownloadTable', "Download Table"),
                              tags$style(type='text/css', "#LasDownloadTable { margin-top: 25px}")
                              ),
                       column(3, 
                              selectInput("LasDownloadType", "File Type", c("pdf", "png"))
                              )
                     ),
                     hr(),
                     
                     wellPanel(
                       
                       textInput("LasPlotTitleT", "Title of plot:", value = "Temperature vs Time"),
                       uiOutput("LasHeatTimeIn"),
                       h4("Sample Heating Rate (Degrees C/second):"),
                       textOutput("LasHeatRate"),
                       h4("Sample Cooling Rate (Degrees C/second):"),
                       textOutput("LasCoolRate"),
                       h4("Sample Mean Holding Temperature (Degrees C):"),
                       h5("This will show as 'NaN' if the heating/holding input time is greater than the time when the laser power shuts off"),
                       textOutput("LasHoldTemp")
                     ),
                     
                     wellPanel(
                       
                       textInput("LasPlotTitleP", "Title of plot:", value = "Laser Power vs Time")
                     )
             )       
           )
  )# close Levitation Laser Plot tab
)# close Levitation Laser tabset panel