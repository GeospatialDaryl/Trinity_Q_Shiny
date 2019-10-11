library(ggplot2)
library(lubridate)




MakeYMDinHYDF <- function(inHYDF, hydYear){
  lenDF <- dim(inHYDF)[1]
  # make vector of dates
  inYear <- hydYear - 1
  diffYear <- years(inYear - 2018) # back in time is negative
  #print(class(diffYear))
  dateV <- rep(NA, lenDF)
  for(i in 1:lenDF){
    dateV[i] <- as.Date(inHYDF$Date[i]) + diffYear
    
  }
  inHYDF$YMD <- as.Date(dateV, origin = lubridate::origin)
  inHYDF$Y <- year(inHYDF$YMD)
  inHYDF$M <- month(inHYDF$YMD)
  inHYDF$D <- day(inHYDF$YMD)
  inHYDF$Date <- inHYDF$YMD
  return(inHYDF)
}

MakeRODHydroYear <- function(RODCat, hydYear){
  #Ex.Wet   Wet  Normal   Dry   Crit.Dry
  #vectPoss <- c("Ex.Wet","Wet","Normal","Dry","Crit.Dry")

  thisYear <- hydYear
  if(RODCat == "Ex.Wet" ){
    vectQ <- ROD_Hydrographs$ROD.Ex.Wet.cfs} 
  if (RODCat == "Wet"){
    vectQ <- ROD_Hydrographs$ROD.Wet.cfs} 
  if (RODCat == "Normal"){
    vectQ <- ROD_Hydrographs$ROD.Normal.cfs}
  if (RODCat == "Dry"){
    vectQ <- ROD_Hydrographs$ROD.Dry.cfs}
  if (RODCat == "Crit.Dry"){
   vectQ <- ROD_Hydrographs$ROD.Crit.Dry.cfs}
  
  outDF <- ROD_Hydrographs[,c(1:2,8)]
  outDF$Q <- vectQ
  MakeYMDinHYDF(outDF, thisYear) -> outDF
  
  if(leap_year(hydYear)){
    outDF <- AddLeapYearDay(outDF)
  }
  outDF <- as_tibble(outDF)
  outDF2 <- select(outDF, DoY,YMD, Q)
  
  return(outDF2)
}





GetProperQdf <- function(inDate){
  #  Returns the correct data frame for date
  out = 0
  if(inDate < histQ.StartDate){
    out <- -1 
  } else if(inDate < histQ.EndDate){
    out <- histQ  
  } else if(inDate < usgsQ.EndDate){
    out <- usgsQ  
  } else if (inDate < trrpQ0218.EndDate){
    out <- trrpQ0218
  } else { out <- -2}
  return(out)
}

GetQ <- function(inDate){
  df <- GetProperQdf(inDate)
  indx <- GetIndexFromDate(df,inDate)
  return(df$Q[indx])
  
}

GetQdf <- function(inHYDF,inDate){
  indx <- GetIndexFromDate(inHYDF,inDate)
  return(inHYDF$Q[indx])
  
}


AddLeapYearDay <- function(inHYDF){

  if(!("Y" %in% colnames(inHYDF))){inHYDF$Y <- NA}
  
  if(!("M" %in% colnames(inHYDF))){inHYDF$M <- NA}
  
  if(!("D" %in% colnames(inHYDF))){inHYDF$D <- NA}
  
  HY99 <- inHYDF[300,]  
  HY99$DoY <- 1000
  HY99$M <- 2
  HY99$D <- 29
  Q1 <- inHYDF$Q[151]
  Q2 <- inHYDF$Q[152]
  HY99$Q <- (Q1 + Q2)/2
  nuDate <- paste(HY99$Y,HY99$M,HY99$D)
  #print(nuDate)
  
  HY99$YMD <- ymd(nuDate)
  print(dim(HY99))
  print(dim(inHYDF))
  #  151 & 152
  outDF <- rbind(inHYDF,HY99)
  outDF <- outDF[order(outDF$YMD),]
  outDF$DoY <- seq(1,366)
  row.names(outDF) <- seq(1,366)
  return( outDF )
  
}

GetIndexFromDate <- function(inHY,inDate){
  startDate <- min(inHY$YMD)
  endDate <- max(inHY$YMD)
  out <- 1
  if(inDate < startDate){ out <- -1 }
  if(inDate > endDate){ out <- -2 }
  if(out > 0){
    out <- inDate - startDate
  }
  return(as.integer(out)+1)
}

plotHydrograph_HYYear <- function(inHYDF){
  #if(!("CFS" %in% colnames(inHYDF))){inHYDF$CFS <- inHYDF$Q}
  #if(!("YMD" %in% colnames(inHYDF))){inHYDF$YMD <- inHYDF$Date}
  #if(!("HY" %in% colnames(inHYDF))){inHYDF$HY <- as.factor(year(inHYDF$Date)[300])}
  #HY = as.factor(1)
  g2 <- ggplot(inHYDF, aes(inHYDF$YMD, inHYDF$Q))+
    geom_line() +
    xlab("Date") +
    ylab("Streamflow (CFS)") +
    scale_color_manual(values = c("red", "black")) +
    #theme_bw() +
    theme(legend.position = c(0.8, 0.8))
  return(g2)}


GetHydroDF<- function(dateStart, dateEnd){
  #print(names(allQ))
  #allQ %>%
    dplyr::filter(tAllQ, YMD < dateEnd + 1) -> in1
    dplyr::filter(in1,YMD > dateStart - 1) -> outDF
  return(outDF)
}


Plot_tAllQ_FullFat <- function(dateStart, dateEnd,
                               boolBaseFlow = FALSE,
                               boolCenterOfMass = FALSE,
                               boolRODHydr = FALSE,
                               boolCCkHydr = FALSE
                               ){
  
  dplyr::filter(tAllQ, YMD < dateEnd + 1) -> in1
  dplyr::filter(in1,YMD > dateStart - 1) -> inDF
  rm(in1)
  
  plotH <- plotHydrograph_HYYear(inDF)
  if( boolCenterOfMass ){
    centerDate <- CalculateCenterofMass(inDF)
    plotH <- plotH + geom_vline(xintercept = as.double(centerDate),
                                linetype = "dashed",
                                color = "green"
    )
  }
  if( boolBaseFlow ){
    plotH <- plotH + geom_line(aes(x=YMD, y=baseQ),
                               linetype = "dashed",
                               color = "blue"
    )
  }
  
  if( boolRODHydr ){
    #  1.  Add to plot
    plotH <- plotH + geom_line(aes(x=YMD, y=ROD_Q),
                               linetype = "dashed",
                               color = "red"
    )
    
    # boolCCkHydr  
  }
  if( boolCCkHydr ){
    #  1.  Add to plot
    plotH <- plotH + geom_line(aes(x=YMD, y=CoffeeCreek.Q),
                               linetype = "solid",
                               color = "purple"
    )
  }
  plot(plotH)
  
  
}

DF_tAllQ_FullFat <- function(dateStart, dateEnd,
                               boolBaseFlow = FALSE,
                               boolCenterOfMass = FALSE,
                               boolRODHydr = FALSE,
                               boolCCkHydr = FALSE
){
  
  dplyr::filter(tAllQ, YMD < dateEnd + 1) -> in1
  dplyr::filter(in1,YMD > dateStart - 1) -> inDF
  rm(in1)
  return(inDF)
}


#plotHydrograph(testROD)






