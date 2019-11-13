# setting up the list of calculations I want to do
serv_calc <- list()
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

hydrogrDF <- function(in1, in2, inDate1, inDate2){
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

hydrogrDF <- function(inDate1, inDate2){
  # 0 . prep the integer date
  # 1. startDate is now YMD of day one of HY
  startDate <- inDate1
  
  # 2. render inDF of target HY
  endDate <- inDate2
  inDF <- GetHydroDF(startDate, endDate)    
  
  # 1. Fetch the select ROD Hydrograph
  
  RODhydrograph <- MakeRODHydroYear(input$rodHY, thisHY)
  nm <- c("ROD_DoY", "ROD_YMD","ROD_Q")
  names(RODhydrograph) <- nm
  inDF <- bind_cols(inDF,RODhydrograph)
  if(VERBOSE){inDF$RODtype<-input$rodHY}
  return(inDF) 
}

# I only want to build a scatterplot when I click my build button, so my list will be of length 1
serv_calc[[1]] <- function(calc, sess){

  calc$hydrographDF <- reactive({
    if (is.null(calc$hydrographDF)) {
      date1 <- ymd("1920-10-01")
      date2 <- ymd("1921-09-30")
      
    } else{
      
    }


    goodDF(input$rodHY,input$singleHY)
  
  })
  }

