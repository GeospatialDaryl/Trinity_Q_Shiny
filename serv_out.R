# set up our serv_out list
serv_out <- list()

# we're just rendering our scatter plot based on the iris dataset
# note the name is the same as the outputid
serv_out[["Hydrograph"]] <- function(calc, sess){
  renderPlot({
    
    
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
  })
}
  
  # renderPlot({
  #   # we add this check to make sure our plot doesn't try to render before we've ever pressed "Build!"
  #   if (!is.null(calc$sub.df)){
  #     # build scatterplot
  #     ggplot(calc$sub.df, aes(x, y, color = factor(species)))+
  #       geom_point()+ # make scatter
  #       ggtitle("Iris Comparisons")+ # add title
  #       xlab(calc$x_axis)+ # change x axis label
  #       ylab(calc$y_axis)+ # change y axis label
  #       labs(color="species")+ # change legend label
  #       NULL
  #   }
  # })
  
