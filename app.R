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
         sliderInput("start",
                     "Hydrologic Year:",
                     min = 1912,
                     max = 2018,
                     value = 106)#,
         #sliderInput("end",
        #             "Hydrologic Year:",
        #             min = 1913,
        #             max = 2018,
        #             value = 105)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
  #  x    <- faithful[, 2] 
  #  bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
  #  hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #}) 
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2] 
      startDate <- ymd(paste(input$start,"-10-01"))
      #endDate <- ymd(paste(input$end,"-10-01"))
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      strDate <- as.Date(startDate)
      inDF <- GetHydroDF(startDate, startDate + 365)
      plotHydrograph_HYYear(inDF)
      
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

