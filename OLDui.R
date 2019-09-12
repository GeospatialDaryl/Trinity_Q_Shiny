ui <- fluidPage(
  
  # Application title
  titlePanel("Trinity River Q"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("daterangeIn", "Date range:",
                     start  = "2001-01-01",
                     end    = "2010-12-31",
                     min    = allQ.StartDate,
                     max    = allQ.EndDate,
                     format = "mm/dd/yy",
                     separator = " - "),
      actionButton("renderDateRange", "Plot Range"),
      sliderInput("singleHY",
                  "Hydrologic Year:",
                  min = 1912,
                  max = 2018,
                  value = 1920,
                  sep = ""),
      checkboxInput("ShowCenterofMass", "Display Center of Mass", FALSE) ,
      checkboxInput("ShowBaseflow", "Display Baseflow", FALSE) #,
      #verbatimTextOutput("value")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
