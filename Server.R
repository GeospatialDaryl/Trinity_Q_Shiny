# Define server logic required to draw a histogram
server <- function(input, output) {
  #output$distPlot <- renderPlot({
  #  input$renderDateRange
  #  startDate <- input$daterangeIn[1]
  #  endDate <- input$daterangeIn[2]
  #    inDF <- GetHydroDF(startDate, endDate)
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    #startDate <- ymd(paste(input$start,"-10-01"))
    #endDate <- ymd(paste(input$end,"-10-01"))
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #strDate <- input$start
    
    
    startDate <- as.integer(input$singleHY)
    startDate <- ymd(p(as.character(startDate),"-10-01"))
    #endDate <- ymd(p(as.character(startDate+1),"-10-01"))
    endDate <- startDate + years(1)
    inDF <- GetHydroDF(startDate, endDate)
    plotH <- plotHydrograph_HYYear(inDF)
    if( input$ShowCenterofMass ){
      centerDate <- CalculateCenterofMass(inDF)
      plotH <- plotH + geom_vline(xintercept = as.double(centerDate),
                                  linetype = "dashed",
                                  color = "red"
      )
    }
    
    plot(plotH)
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}
