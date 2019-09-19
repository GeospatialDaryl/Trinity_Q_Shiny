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

# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/ecdf.html
#-- Simple didactical  ecdf  example :
#x <- rnorm(12)

predamECDF <- ecdf(yrlyQ)

# Fn <- ecdf(x)
# Fn     # a *function*
# Fn(x)  # returns the percentiles for x
# tt <- seq(-2, 2, by = 0.1)
# 12 * Fn(tt) # Fn is a 'simple' function {with values k/12}
# summary(Fn)
# ##--> see below for graphics
# knots(Fn)  # the unique data values {12 of them if there were no ties}
# 
# y <- round(rnorm(12), 1); y[3] <- y[1]
# Fn12 <- ecdf(y)
# Fn12
# knots(Fn12) # unique values (always less than 12!)
# summary(Fn12)
# summary.stepfun(Fn12)
# 
# ## Advanced: What's inside the function closure?
# ls(environment(Fn12))
# ## "f"     "method" "na.rm"  "nobs"   "x"     "y"    "yleft"  "yright"
# utils::ls.str(environment(Fn12))
# stopifnot(all.equal(quantile(Fn12), quantile(y)))