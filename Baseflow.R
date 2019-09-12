#https://rdrr.io/cran/EcoHydRology/man/hydrograph.html
library(EcoHydRology)
bfs1912 <- BaseflowSeparation( HY1912$Q, passes = 2, filter_parameter = 0.925)
hydrograph(input = cbind(HY1912$YMD,HY1912$Q) , streamflow2 = bfs1912$bt)
