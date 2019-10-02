library(peakPick)
peakpick(mQ, 7,deriv.lim=0.01, peak.min.sd=.5, peak.npos=1L) -> peaks
plot(tHY1912$YMD, peaks[,1]) 
par(new=T)
plot(tHY1912$YMD, tHY1912$Q, "l") 
par(new=F)

