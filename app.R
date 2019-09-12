#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)

#  Data Summary:

# 1911-10-01 # HY12
#       `histQ` - tidy historic Trinity Q 
#             RC:  17898 x 7
#           from:  1911-10-01  HY12
#             to:  1960-09-30  HY60
# [1] "DoY" "M"   "D"   "Y"   "Q"   "WY"  "YMD"
# 1960-09-30 # HY60

# 1959-07-22 # HY59
#      `usgsQ` - tidy USGS @ Lewiston 
#             RC:  17177 x 13
#           from:  1959-07-22  HY60
#             to:  2006-07-31  HY05
#[1] "Agency_Code"        "Site_No"            "Date"              
#[4] "TempMax_degC"       "TempCode"           "TempMin_degC"      
#[7] "TempMinCode"        "CFS"                "QCode"             
#[10] "SuspSed_mgpL"       "SuspSedMGPLCode"    "SuspSed_tonsPerDay"
#[13] "SuspSedTonsCode"   "Q"
#
# 2006-07-31 # HY06

# 2001-10-01 # HY02
#      `trrpQ0218` - tidy nu school Trinity Q 
#             RC:  6205 x 8
#           from:  2001-10-01  HY02
#             to:  2018-09-30  HY18
# [1] "DoY" "M"   "D"   "Y"   "Q"   "WY"  "YMD"
# 2018-09-30 # HY18


# Define UI for application that draws a histogram
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
      checkboxInput("ShowBaseflow", "Display Baseflow", FALSE)    ,
      radioButtons("rodHY", "ROD Flow Year:",
                   c("Ex. Wet" = "ex.wet",
                     "Wet" = "wet",
                     "Normal" = "norm",
                     "Dry" = "dry",
                     "Crit. Dry" = "crit.dry")),
      checkboxInput("boolRODHydr", "Show ROD Hydrograph", FALSE)    #,
      #verbatimTextOutput("value")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # 0 . prep the integer date
    thisHY <- as.integer(input$singleHY)   # int HY
    startDate <- thisHY - 1  
    iStartYear <- startDate  # integer start Year
    # 1. startDate is now YMD of day one of HY
    startDate <- ymd(p(as.character(startDate),"-10-01"))

    # 2. render inDF of target HY
    endDate <- startDate + years(1)
    inDF <- GetHydroDF(startDate, endDate - 1)    
    
    # 1. Fetch the select ROD Hydrograph
    if(input$boolRODHydr){
      RODhydrograph <- MakeRODHydroYear(input$rodHY, thisHY)
      nm <- c("ROD_DoY", "ROD_YMD","ROD_Q")
      names(RODhydrograph) <- nm
      inDF <- bind_cols(inDF,RODhydrograph)
    }
    

    plotH <- plotHydrograph_HYYear(inDF)
    if( input$ShowCenterofMass ){
      centerDate <- CalculateCenterofMass(inDF)
      plotH <- plotH + geom_vline(xintercept = as.double(centerDate),
                                  linetype = "dashed",
                                  color = "red"
      )
    }
    if( input$ShowBaseflow ){
      plotH <- plotH + geom_line(aes(x=YMD, y=baseQ),
                                 linetype = "dashed",
                                 color = "blue"
      )
    }
    
    
    if( input$boolRODHydr ){
      #  0. Create ROD HY
      #  1.  Add to plot
      RODhydrograph <- MakeRODHydroYear(input$rodHY, thisHY)
      inDF$ROD_Q <- RODhydrograph
      plotH <- plotH + geom_line(aes(x=YMD, y=ROD_Q),
                                 linetype = "dashed",
                                 color = "red"
      )
      
    }
    plot(plotH)
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)