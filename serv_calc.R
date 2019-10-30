# setting up the list of calculations I want to do
serv_calc <- list()

# I only want to build a scatterplot when I click my build button, so my list will be of length 1
serv_calc[[1]] <- function(calc, sess){
  # this is going to activate any time I press "build!"
  #observeEvent(calc$go, {
    # create our data frame for visualizing
  #  sub.df <- data.frame("x" = iris[iris$Species %in% calc$spec,calc$x_axis],
  #                       "y" = iris[iris$Species %in% calc$spec,calc$y_axis],
  #                       "species"=iris[iris$Species %in% calc$spec,"Species"])
    
    # add this to calc, since we want to use this in our rendering
   
  #  the reactDF
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
   calc[["hydrographDF"]] <- goodDF
  #})
}
