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
library(tibble)
library(dplyr)
library(ggplot2)
library(rhandsontable)
library(EcoHydRology)
VERBOSE=TRUE

#####  Data Summary:  ############

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

# Define server logic required to draw a histogram

####  CORE FUNCTIONS  ######
FixThatReactiveDT <- function(inReac){
  outDF <- inReac[,c(1, 6, 3,2,4,5,8)]
  names(outDF) <- c("Date", "DoY"," HY", "Q", "Baseflow", "Transient", "ROD Q"  )
  return(outDF)
}

MakeEmptyHydrographDF <- function(){
  thisDF <- data.frame(YMD=as.Date(character()),
                       Q=double(),
                       Note = character(),
                       stringsAsFactors = FALSE
                       )
  return(thisDF)
}


#####   CORE SERVER   #########

server <- function(input, output) {
  
  reactDF <- reactive({
    goodDF <- function(in1, in2){
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
      
      RODhydrograph <- MakeRODHydroYear(input$rodHY, thisHY)
      nm <- c("ROD_DoY", "ROD_YMD","ROD_Q")
      names(RODhydrograph) <- nm
      inDF <- bind_cols(inDF,RODhydrograph)
      if(VERBOSE){inDF$RODtype<-input$rodHY}
      return(inDF)
    }
    goodDF(input$rodHY,input$singleHY)
  })
  
  observeEvent(input$launchEditor,{
    thisHydrograph <- MakeEmptyHydrographDF()
    editTable(thisHydrograph)
  })
  
  output$tableOut <- renderDataTable(FixThatReactiveDT(reactDF()),
                                     options = list(pageLength = 10)
  )  
  
  output$distPlot <- renderPlot({
    
    
    inDF <- reactDF()
    plotH <- plotHydrograph_HYYear(inDF)
    if( input$ShowCenterofMass ){
      centerDate <- CalculateCenterofMass(inDF)
      plotH <- plotH + geom_vline(xintercept = as.double(centerDate),
                                  linetype = "dashed",
                                  color = "green"
      )
    }
    if( input$ShowBaseflow ){
      plotH <- plotH + geom_line(aes(x=YMD, y=baseQ),
                                 linetype = "dashed",
                                 color = "blue"
      )
    }
    
    if( input$boolRODHydr ){
      #  1.  Add to plot
      plotH <- plotH + geom_line(aes(x=YMD, y=ROD_Q),
                                 linetype = "dashed",
                                 color = "red"
      )
    
      # boolCCkHydr  
    }
    if( input$boolCCkHydr ){
      #  1.  Add to plot
      plotH <- plotH + geom_line(aes(x=YMD, y=CoffeeCreek.Q),
                                 linetype = "solid",
                                 color = "purple"
      )
      
      # boolCCkHydr  
    }
    
    if( input$boolUnimpededHydr ){
      #  1.  Add to plot
      plotH <- plotH + geom_line(aes(x=YMD, y=TriQ.pred),
                                 linetype = "solid",
                                 color = "navy" )
                            }
    plot(plotH)
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = fpVersionZero, server = server)