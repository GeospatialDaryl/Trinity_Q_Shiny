library(rhandsontable)


##### fluidPage - VersionZero   #####
#  Original call - flat shiny
#
#ui <- fluidPage(
fpVersionZero <- fluidPage(
  #theme
  #theme = "bootstrap.lumen.css",
  
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
      checkboxInput("boolRODHydr", "Show ROD Hydrograph", FALSE)    ,
      checkboxInput("boolCCkHydr", "Show Coffee Creek Hydrograph", FALSE),
      checkboxInput("boolUnimpededHydr", "Show Unimpeded Hydrograph", FALSE) 
      #verbatimTextOutput("value")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      #  Tabset Output
      tabsetPanel(type="tabs",
                  tabPanel("Plot", plotOutput("distPlot")),
                  tabPanel("Table", dataTableOutput("tableOut")),
                  tabPanel("Input Hydrograph", 
                           textInput("Hydrograph Name", "thisHyName", 
                                     value = "myHydrograph", width = NULL,
                                     placeholder = NULL),
                           actionButton("launchEditor","Launch Editor"),
                           rHandsontableOutput("hot") )
                  
      )
      #plotOutput("distPlot")
      #dataTableOutput("tableOut")
    )
  )
)



##### fluidPage - only Hydrograph v0 #####
fpHydro_v0 <- fluidPage(
  #theme
  #theme = "bootstrap.lumen.css",
  
  # Application title
  titlePanel("Trinity River Q"),
  

    # Show a plot of the generated distribution
    mainPanel(
      
      #  Tabset Output
      tabsetPanel(type="tabs",
                  tabPanel("Plot", plotOutput("distPlot")),
                  tabPanel("Table", dataTableOutput("tableOut"))

                  
      )
    )
  )
