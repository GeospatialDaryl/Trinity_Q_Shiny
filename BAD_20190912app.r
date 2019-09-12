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
VERBOSE=TRUE
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
                   c("Ex. Wet" = "Ex.Wet",
                     "Wet" = "Wet",
                     "Normal" = "Normal",
                     "Dry" = "Dry",
                     "Crit. Dry" = "Crit.Dry")),
      checkboxInput("RODhydr", "Show ROD Hydrograph", FALSE)    #,
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
    if( input$RODhydr){
      if(VERBOSE){print(c(year(endDate),class(year(endDate))))}
      #  0. Create ROD HY
      #RODhydrograph <- MakeRODHydroYear(input$rodHY, as.integer(year(endDate)))

      #print(c("dim of RODhydr", dim(RODhydrograph)))
      print(c("dim of inDF", dim(inDF)))
      
    }

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
    if( input$ShowBaseflow ){
      plotH <- plotH + geom_line(aes(x=YMD, y=baseQ),
                                 linetype = "dashed",
                                 color = "blue"
      )
    }
    if( input$RODhydr ){

      
      nm <- c("ROD_DoY", "ROD_YMD","ROD_Q")
      names(RODhydrograph) <- nm
      if(VERBOSE){print(RODhydrograph)}
        
      inDF <- bind_cols(inDF,RODhydrograph)
      if(VERBOSE){print(inDF)}
      
      #  1.  Add to plot
      if(VERBOSE){print(dim(RODhydrograph))}
      plotH <- plotH + geom_line(aes(x=YMD, y=ROD_Q),
                                 linetype = "dashed",
                                 color = "green"
      )

    }
    plot(plotH)
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)