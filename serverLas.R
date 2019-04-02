# This block of code recognizes when the user selects an input file
LasRawData <- reactive({
  inFile <- input$LasFile
  
  if(is.null(inFile))
    return(NULL)

  tryCatch(
    {
    read_excel(inFile$datapath, skip = 121)
    },
    error = function(cond){read_tsv(inFile$datapath, skip = 121)}
    )

})

output$LasHeatTimeIn <- renderUI({
  
  req(LasMain$PlotData)
  data <- subset(LasMain$PlotData, LasMain$PlotData$Time >= min(LasMain$PlotData$Time))

  numericInput("LasHeatRateTime", "Input time where heating becomes holding (s):",
               value = data$Time[length(which(data$Laser == "On"))],
               # min = min(data$Time),
               max = data$Time[length(which(data$Laser == "On"))],
               step = 0.1)
})


# This block of code reads the file input to pull basic information
pullInfo <- reactive({
  inFile <- input$LasFile
  
  tryCatch(
    {
      # Currently not showing the time as a variable
      file <- data.frame(read_excel(inFile$datapath, col_names = FALSE))
      foo <- list()
      foo$gasA <- as.character(file[104,2])
      foo$gasARatio <- (100 - as.numeric(file[81,2]))
      foo$gasB <- as.character(file[105,2])
      foo$gasBRatio <- as.numeric(file[81,2])
      foo$gasFlowRate <- as.numeric(file[78,2])
      foo$dateTime <- format(as.POSIXct(as.Date(as.numeric(file[121,1]), origin = "1899-12-31"), digits = 10), "%m/%d/%Y")
      return(foo)
    },
    error = function(cond){
      # Currently not showing the time as a variable
      file <- data.frame(read_tsv(inFile$datapath, col_names = FALSE))
      foo <- list()
      foo$gasA <- as.character(file[104,2])
      foo$gasARatio <- (100 - as.numeric(file[81,2]))
      foo$gasB <- as.character(file[105,2])
      foo$gasBRatio <- as.numeric(file[81,2])
      foo$gasFlowRate <- as.numeric(file[78,2])
      foo$dateTime <- (file[121,1])
      return(foo)
      }
  )
})

output$LasGasFlow <-  renderText({
  pullInfo()$gasFlowRate
  })

# This block of code builds a reactiveValues object to store the input data
LasMain <- reactiveValues(data = list(NULL))

# This block of code stores the raw input data into a reactiveValues object as $Raw
observeEvent(input$LasFile, {
  temp <- LasRawData()
  LasMain$Data <- temp[which(temp$'Corrected Temp' >= 600), ]

})

# This block of code pre-renders user input boxes that
# allow the user to select which column is their Temp, Pressure, and grouping variables.
output$LasChoose_A <- renderUI({
  selectizeInput("LasX", "Select Time variable:", 
                 choices = as.list(c("", colnames(LasMain$Data))), 
                 selected = "Time", 
                 multiple = FALSE, 
                 options = list(maxItems = 1))
})

output$LasChoose_B <- renderUI({
  selectizeInput("LasY", "Select Temperature variable:", 
                 choices = as.list(c("", colnames(LasMain$Data))), 
                 selected = "Corrected Temp", 
                 multiple = FALSE, 
                 options = list(maxItems = 1))
})

output$LasChoose_C <- renderUI({
  selectizeInput("LasY2", "Select Laser Power variable:",
                 choices = as.list(c("", colnames(LasMain$Data))),
                 selected = "Laser Power",
                 multiple = FALSE,
                 options = list(maxItems = 1))
})

# This block of code builds the plot data set
observeEvent( c(input$LasFile, input$LasX, input$LasY, input$LasY2), {

  if(input$LasX != "" & input$LasY != "" & input$LasY2 != "" & !is.null(input$LasFile)) {
    
    names <- colnames(LasMain$Data)
    data <- LasMain$Data
    
    x <- data[,which(names == input$LasX)]
    y <- data[,which(names == input$LasY)]
    y2 <- data[,which(names == input$LasY2)]
    Laser <- as.numeric(data$'Laser Power' > 0)
    LaserOn <- max(which(diff(Laser) != 0))
    LaserOff <- length(Laser) - LaserOn
    Laser <- factor(c(rep("On", LaserOn), rep("Off", LaserOff)))
    Laser <- factor(Laser,levels(Laser)[c(2,1)])

    temp <- data.frame(Laser = Laser, x, y, y2)
    names(temp) <- c("Laser", "Time", "Temp", "Power")
    
    LasMain$PlotData <- temp
  }

})

# This block of code renders the raw data and plot data to the UI
output$LasFullData <- renderDataTable({ LasMain$Data })
output$LasPlotData <- renderDataTable({ LasMain$PlotData })

# This block of code runs a linear regression where the laser power is off and
# returns the regression model
LasCoolReg <- reactive({
  data <- subset(LasMain$PlotData, LasMain$PlotData$Laser == "Off")
  fit <- lm(data = data, Temp~Time)
  fit
})

# This block of code runs a linear regression where the laser power is on and the temperature
# is less than the max laser "On" time (or a different user input) and returns the regression model
LasHeatReg <- reactive({
  data2 <- subset(LasMain$PlotData, LasMain$PlotData$Laser == "On")
  req(input$LasHeatRateTime)
  if(input$LasHeatRateTime > min(LasMain$PlotData$Time)){
    data2 <- subset(data2, Time < input$LasHeatRateTime)
    fit2 <- lm(data = data2, Temp~Time)
    fit2
  } else {
    NULL
  }
})

# This block of code calculates the derivative over the heating range and then
# averages those values to determine the average heating rate
LasHeat <- reactive({
  data <- subset(LasMain$PlotData, (LasMain$PlotData$Laser == "On") & (Time < input$LasHeatRateTime))
  div1 <- c(diff(data$Temp)/diff(data$Time))
  HeatRate <- abs(mean(div1))
  HeatRate
})

output$LasHeatRate <-  renderText({LasHeat()})

# This block of code calculates the average hold temperature, beginning at the user-defined break between heating and holding, and 
# ending 1 line before the laser shuts off
LasMeanHold <- reactive({
  data <- subset(LasMain$PlotData, (LasMain$PlotData$Time >= input$LasHeatRateTime) & (LasMain$PlotData$Laser != "Off"))
  MeanHoldTemp <- abs(mean(data$Temp))
  MeanHoldTemp
})

output$LasHoldTemp <- renderText({LasMeanHold()})

# This block of code calculates the derivative over the cooling range and then
# averages those values to determine the average cooling rate
LasCool <- reactive({
  data <- subset(LasMain$PlotData, LasMain$PlotData$Laser == "Off")
  div1 <- c(diff(data$Temp)/diff(data$Time))
  coolRate <- abs(mean(div1))
  coolRate
})

output$LasCoolRate <-  renderText({LasCool()})

# This section of code calculates variables for this sample run
sampValues <- reactive({
# pullInfo()
  gasFlow <- pullInfo()$gasFlowRate
  maxTemp <- max(LasMain$PlotData$Temp)
  totalTime <- (max(LasMain$PlotData$Time)-min(LasMain$PlotData$Time))
  if(input$LasPlotGas != ""){
    gasFlow <- input$LasPlotGas
  }
  addNotes <- input$LasTableNotes
  sampName <- input$LasFile$name
  sampName <- strsplit(sampName, "\\.xls")[[1]][1]
  sampValues <- data.frame(Sample_Name = sampName, 
                           Date = pullInfo()$dateTime, 
                           Gas_A = pullInfo()$gasA, 
                           Gas_A_Percent = pullInfo()$gasARatio,
                           Gas_B = pullInfo()$gasB, 
                           Gas_B_Percent = pullInfo()$gasBRatio,
                           Gas_Flow = gasFlow, 
                           #PID_Hold_Time = pullInfo()$PIDHold, 
                           Heating_Duration = (input$LasHeatRateTime - min(LasMain$PlotData$Time)),
                           Holding_Duration = (max(LasMain$PlotData$Time) - input$LasHeatRateTime), 
                           Mean_Hold_Temp = LasMeanHold(),
                           Peak_Temp = maxTemp, 
                           Cool_Rate_Deriv = LasCool(),
                           Cool_Rate_Reg = abs(coef(lm(LasCoolReg()))[2]),
                           Heat_Rate_Deriv = LasHeat(),
                           Heat_Rate_Reg = coef(lm(LasHeatReg()))[2],
                           Additional_Notes = addNotes
                           )
  sampValues <- dplyr::rename(sampValues, 
                              'Sample Name' = Sample_Name, 
                              'Gas A' = Gas_A,
                              'Gas A %' = Gas_A_Percent,
                              'Gas B' = Gas_B,
                              'Gas B %' = Gas_B_Percent,
                              'Gas Flow (CC/min)' = Gas_Flow,
                              'Heating Duration (s)' = Heating_Duration,
                              'Holding Duration (s)' = Holding_Duration,
                              'Mean Hold Temp (deg C)' = Mean_Hold_Temp,
                              'Peak Temp (deg C)' = Peak_Temp,
                              'Deriv. Cool Rate (deg C/s)' = Cool_Rate_Deriv,
                              'Reg. Cool Rate (deg C/s)' = Cool_Rate_Reg,
                              'Deriv. Heat Rate (deg C/s)' = Heat_Rate_Deriv,
                              'Reg. Heat Rate (deg C/s)' = Heat_Rate_Reg,
                              'Additional Notes' = Additional_Notes
  )
  return(sampValues)
})

observeEvent(input$question, {
  if(input$question == TRUE)
    showModal(modalDialog(
      title = "Additional Notes Info:",
      "Input additional notes about this sample. These will fill the 'Additional Notes' column of the table output.", 
      footer = modalButton("Dismiss")
    ))
})

# This block of code builds the plot
LasPlotTemp <- reactive({
  
  data <- LasMain$PlotData
  cols <- c(input$LasOnCol, input$LasOffCol )
  
  # Base plot
  LasT <- ggplot(data = data, aes(x = Time, y = Temp, color = Laser)) +
    geom_line(size = input$TempPlotThickness) +
    scale_color_manual(name = "Laser Power", values=cols, labels = c("On", "Off"))
  
  # Add cooling regression line and equation as annotation
  if(input$LasAddCoolReg == TRUE) {
  LasT <- LasT +
    geom_smooth(data = subset(data, Laser == "Off"),
                method = lm, se = FALSE, color = "black", linetype = "dashed") +
    annotate(geom = "text",
             x = max(data$Time)*.95, y = min(data$Temp),
             label = lm_eqn(LasCoolReg()),
             colour = input$LasOffCol,
             family="sans",
             size = 5, parse = TRUE, hjust = 1)
  }
  
  # Add heating regression line and equation as annotation
  if(input$LasAddHeatReg == TRUE & input$LasHeatRateTime > min(data$Time) & is.null(LasHeatReg()) == FALSE) {
    LasT <- LasT +
      geom_smooth(data = subset(data, (Laser == "On") & (Time < input$LasHeatRateTime)),
                  method = lm, se = FALSE, color = "black", linetype = "dashed") +
      annotate(geom = "text",
               x = max(data$Time)*.95, y = min(data$Temp) - 80,
               label = lm_eqn(LasHeatReg()),
               colour = input$LasOnCol,
               family="sans",
               size = 5, parse = TRUE, hjust = 1)
  }
  
  # Output final plot with final additions
  LasT +
    ggtitle(input$LasPlotTitleT) +
    xlab("Time [Seconds]") +
    ylab( expression(paste("Temperature [",degree,"C]")) )+
    theme_classic() +
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    coord_cartesian(expand = TRUE, 
                    xlim = c(LasMain$zoomTemp$xmin, LasMain$zoomTemp$xmax),
                    ylim = c(LasMain$zoomTemp$ymin, LasMain$zoomTemp$ymax))

})  

LasPlotPower <- reactive({
  
  data2 <- LasMain$PlotData

  # Base plot
  LasP <- ggplot(data = data2, aes(x = Time, y = Power)) +
    geom_line(size = input$PowerPlotThickness, color = input$LasPowerCol) 

  # Output final plot with final additions
  LasP +
    ggtitle(input$LasPlotTitleP) +
    xlab("Time [Seconds]") +
    ylab(paste("Laser Power")) +
    theme_classic() +
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    coord_cartesian(expand = TRUE, 
                    xlim = c(LasMain$zoomPower$xmin, LasMain$zoomPower$xmax),
                    ylim = c(LasMain$zoomPower$ymin, LasMain$zoomPower$ymax))
})

# This block of code renders the plot and sends it to the UI as an output
output$LasPlotTemp <- renderPlot({ LasPlotTemp() })
output$LasPlotPower <- renderPlot({ LasPlotPower() })

# This block of code allows the mouse hovering over the plot to show the x and y coordinates for the Temp vs Time
output$infoTemp <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("Time (s) = ", round(e$x, 2), " Temp (C) = ", round(e$y, 2), "\n")
  }
  xy_range_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("xmin=", round(e$xmin, 2), " xmax=", round(e$xmax, 2), 
           " ymin=", round(e$ymin, 2), " ymax=", round(e$ymax, 2))
  }
  paste0(
    "hover: ", xy_str(input$LasPlotTemp_hover)
  )
  
})

################ Code For Plot Interactivity ################

# This block of code resets the plot xmin, xmax, ymin, ymax on a brush event for the temperature plot
observeEvent(input$LasPlotTemp_brush, {
  
  brush <- input$LasPlotTemp_brush
  if (!is.null(brush)) {
    LasMain$zoomTemp <- list(xmin = brush$xmin, xmax = brush$xmax, ymin = brush$ymin, ymax = brush$ymax)
  }
  
})

# This block of code resets the plot xmin, xmax, ymin, ymax on a brush event for the power plot
observeEvent(input$LasPlotPower_brush, {
  
  brush <- input$LasPlotPower_brush
  if (!is.null(brush)) {
    LasMain$zoomPower <- list(xmin = brush$xmin, xmax = brush$xmax, ymin = brush$ymin, ymax = brush$ymax)
  }
  
})

# This block of code resets the zoom to null on a temperature plot dblclick
observeEvent(c(input$LasPlotTempDblClick, input$LasHeatRateTime), {
  # browser()
  data <- LasMain$PlotData
  if(is.null(LasHeatReg()) == TRUE){
    ymax <- max(data$Temp)
  } else {
    ymax <- max(c(max(predict(LasHeatReg())), max(data$Temp)))
  }
  ymin <- min(c(min(predict(LasCoolReg())), min(data$Temp)))

  LasMain$zoomTemp <- list(xmin = min(data$Time), xmax = max(data$Time), 
                           ymin = ymin, ymax = ymax)
})

# This block of code resets the zoom to null on a power plot dblclick
observeEvent(input$LasPlotPowerDblClick, {
  data <- LasMain$PlotData
  LasMain$zoomPower <- list(xmin = min(data$Time), xmax = max(data$Time), ymin = min(data$Power), ymax = max(data$Power))
})
############################################################

# This block of code allows the mouse hovering over the plot to show the x and y coordinates for the Power vs Time
output$infoPower <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("Time (s) = ", round(e$x, 2), " Laser Power = ", round(e$y, 2), "\n")
  }
  xy_range_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("xmin=", round(e$xmin, 2), " xmax=", round(e$xmax, 2), 
           " ymin=", round(e$ymin, 2), " ymax=", round(e$ymax, 2))
  }
  paste0(
    "hover: ", xy_str(input$LasPlotPower_hover)
  )
  
})

# This block of code creates a download handler and downloads the Temp plot
output$LasDownloadTPlot <- downloadHandler(
  
  filename =  function() {
    paste("MyLaserPlot_T", input$LasDownloadType, sep=".")
  },
  
  content = function(file) {
    ggsave(file, plot = LasPlotTemp(), width = 10, height = 7, device = input$LasDownloadType)
  }
)

# This block of code creates a download handler and downloads the Power plot
output$LasDownloadPPlot <- downloadHandler(
  
  filename =  function() {
    paste("MyLaserPlot_P", input$LasDownloadType, sep=".")
  },
  
  content = function(file) {
    ggsave(file, plot = LasPlotPower(), width = 10, height = 7, device = input$LasDownloadType)
  }
)

# This block of code creates a download handler and downloads a csv of sample data
# the written file takes the output of the reactive function sampValues() and applies it to a csv
output$LasDownloadTable <- downloadHandler(
  
  filename =  function() {
    paste("Sample Information", ".csv", sep = "")
  },
  
  content = function(file) {
    write.csv(sampValues(), file, row.names = FALSE)
  }

)

# This block of code creates a download handler to download a sample data set
output$sampleDataDownload <- downloadHandler(
  filename <- function() {
    "Lev_sample_data.xls"
  },
  
  content <- function(file) {
    file.copy("TestData/Lev_sample_data.xls", file)
  }
)
