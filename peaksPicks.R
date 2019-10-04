library(peakPick)

mQ <- cbind(HY1912$Q,HY1912$Q)
#peakpick(mQ, 7,deriv.lim=0.01, peak.min.sd=.5, peak.npos=1L) -> peaks
peakpick(mQ, 7,deriv.lim=0.01, peak.min.sd=.5, peak.npos=1L) -> peaks
plot(seq(1,366), peaks[,1]) 
par(new=T)
plot(tHY1912$YMD, tHY1912$Q, "l") 
par(new=F)

peakPicker_smooth <- function(inHYDF){
  yrLen <- dim(inHYDF)[1]
  mQ <- cbind(inHYDF$Q,inHYDF$Q)
  peakpick(mQ, 10,deriv.lim=0.01, peak.min.sd=.5, peak.npos=1L) -> peaks
  peaks <- peaks[,2]
  plot(inHYDF$YMD, peaks) 
  par(new=T)
  plot(inHYDF$YMD, inHYDF$Q, "l") 
  par(new=F)
  
}
peakPicker <- function(inHYDF){
  yrLen <- dim(inHYDF)[1]
  mQ <- cbind(inHYDF$Q,inHYDF$Q)
  peakpick(mQ, 10,deriv.lim=0.01, peak.min.sd=.5, peak.npos=1L) -> peaks
  peaks <- peaks[,2]
  plot(inHYDF$YMD, peaks) 
  par(new=T)
  plot(inHYDF$YMD, inHYDF$Q, "l") 
  par(new=F)
  
}

peakPicker(HY1912)