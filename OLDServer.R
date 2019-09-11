# Define server logic required to draw a histogram
server <- function(input, output) {
  #output$distPlot <- renderPlot({
  #  input$renderDateRange
  #  startDate <- input$daterangeIn[1]
  #  endDate <- input$daterangeIn[2]
  #    inDF <- GetHydroDF(startDate, endDate)
  
  output$distPlot <- renderPlot({
    
    
    startDate <- as.integer(input$singleHY)
    startDate <- ymd(p(as.character(startDate),"-10-01"))

    endDate <- startDate + years(1)
    
    inDF <- GetHydroDF(startDate, endDate)
    plotH <- plotHydrograph_HYYear(inDF)
    if( input$ShowCenterofMass ){
      centerDate <- CalculateCenterofMass(inDF)
      plotH <- plotH + geom_vline(xintercept = as.double(centerDate),
                                  linetype = "dashed",
                                  color = "blue"
      )
    }
    if( input$ShowBaseflow ){
      plotH <- plotH + geom_line(data = tAllQ,
                                 aes(x = tAllQ$YMD, y = tAllQ$baseQ),
                                  linetype = "dashed",
                                  color = "red"
      )
    }
    
    plot(plotH)
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}
