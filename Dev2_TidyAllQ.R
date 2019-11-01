summary(tAllQ)

rm(HY1912,HY1913,HY1914,HY1915,HY1916,HY1917,HY1918,HY1919,HY1920,HY1921,HY1922,HY1923,HY1924,HY1925,HY1926,HY1927,HY1928,HY1929,HY1930,HY1931,HY1932,HY1933,HY1934,HY1935,HY1936,HY1937,HY1938,HY1939,HY1940,HY1941,HY1942,HY1943,HY1944,HY1945,HY1946,HY1947,HY1948,HY1949,HY1950,HY1951,HY1952,HY1953,HY1954,HY1955,HY1956,HY1957,HY1958,HY1959,HY1960,HY1961,HY1962,HY1963,HY1964,HY1965,HY1966,HY1967,HY1968,HY1969,HY1970,HY1971,HY1972,HY1973,HY1974,HY1975,HY1976,HY1977,HY1978,HY1979,HY1980,HY1981,HY1982,HY1983,HY1984,HY1985,HY1986,HY1987,HY1988,HY1989,HY1990,HY1991,HY1992,HY1993,HY1994,HY1995,HY1996,HY1997,HY1998,HY1999,HY2000,HY2001,HY2002,HY2003,HY2004,HY2005,HY2006,HY2007,HY2008,HY2009,HY2010,HY2011,HY2012,HY2013,HY2014,HY2015,HY2016,HY2017,HY2018,HY2019)

#####   main tidys   ######
#   tidyTri.Q           Tri.Q
#   tidyTri.baseQ       Tri.baseQ
#   tidyTri.tQ          Tri.tQ
#   tidyCCk.Q           CCk.Q
#   tidyTri.pred.Q      Tri.pred.Q


tidyTri.Q <- tAllQ[,c(1,3,2)]
tidyTri.Q$val <- "Tri.Q"

tidyTri.baseQ <- tAllQ[,c(1,3,4)]
tidyTri.baseQ$val <- "Tri.baseQ"

tidyTri.tQ <- tAllQ[,c(1,3,5)]
tidyTri.tQ$val <- "Tri.tQ"

tidyCCk.Q <- tAllQ[,c(1,3,6)]
tidyCCk.Q$val <- "CCk.Q"

tidyTri.pred.Q <- tAllQ[,c(1,3,7)]
tidyTri.pred.Q$val <- "Tri.pred.Q"

filter(tidyTri.Q, tidyTri.Q$HY == 1912)
tidyTri.Q[tidyTri.Q$HY == 1912,][367,]

group_by(tidyTri.Q, HY) -> groupTidyTri.Q
summarize(groupTidyTri.Q, yrsum = sum(Q)) -> tblYearlySums.Tri.Q

convFctr_CFSD_to_AcreFeet <- (86400)/43559.935
tblYearlySums.Tri.Q$yrsum_acrefeet <- tblYearlySums.Tri.Q$yrsum * convFctr_CFSD_to_AcreFeet

classifyWY_contemporary <- function(inQ, units = "acrefeet"){
  if(!units == "acrefeet"){
    inQ <- inQ * convFctr_CFSD_to_AcreFeet
  }
  if(inQ < 369000){
    return ("Crit.Dry")
  } else if(inQ <= 453000){
    return ("Dry") 
  } else if(inQ <= 647000){
    return ("Normal")
  } else if(inQ <= 701000){
    return ("Wet")
  }  else {return ("Ex.Wet")}
}

classifyWY_historic <- function(inQ, units = "acrefeet"){
  if(!units == "acrefeet"){
    inQ <- inQ * convFctr_CFSD_to_AcreFeet
  }
  if(inQ < 650000){
    return ("Crit.Dry")
  } else if(inQ <= 1025000){
    return ("Dry") 
  } else if(inQ <= 1350000){
    return ("Normal")
  } else if(inQ <= 2000000){
    return ("Wet")
  }  else {return ("Ex.Wet")}
}
##### Now update table  ######
tblYearlySums.Tri.Q$WYType <- ""
  
for(i in seq(1,49)){
tblYearlySums.Tri.Q$WYType[i] <- classifyWY_historic(tblYearlySums.Tri.Q$yrsum_acrefeet[i])
}

for(i in seq(50,107)){
  tblYearlySums.Tri.Q$WYType[i] <- classifyWY_contemporary(tblYearlySums.Tri.Q$yrsum_acrefeet[i])
}