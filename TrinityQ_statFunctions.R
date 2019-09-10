GetHY <- function(inYr){
  inYri <- as.integer(inYr)
  inYr_s <- inYri - 1
  inYr_e <- inYri
  yr_s <- ymd(p(inYr_s,"-10-01"))
  yr_e <- ymd(p(inYr_e,"-09-30"))
  
  filter(allQ, YMD <= yr_e & YMD >= yr_s) -> outDF
  return(outDF)
}


CalculateCenterofMass <- function(inHYDF){
  sum <- 0  # sum of mass
  summ<- 0  # sum of m*r
  lenYr <- dim(inHYDF)[1]
  i <- 0
  k <- 0
  inHYDF$DoY <- seq(1,lenYr)
  
  for(i in seq(1,lenYr)){
    summ <- summ + inHYDF$Q[i]
    sum <- sum + inHYDF$Q[i]*inHYDF$DoY[i]
    #print(sum)
    #print(summ)
  }
  R <- sum/summ
  
  inDay <- min(inHYDF$YMD)
  outDay <- inDay + days(as.integer(round(R))) 
  return(outDay)
}