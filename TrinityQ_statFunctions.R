library(dplyr)

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

#predamECDF <- ecdf(yrlyQ)



fixNA <- function(inNum){
  if(is.na(inNum) ){outNum <- 0.}
  else{ outNum <- inNum}
  return(outNum)
}

seqAcross <- function(inDF){
  lenDF <- dim(inDF)[1]
  return(seq(1,lenDF) )
}


PlotHYwPeaks <- function(inHYDF, inPeaksDF){
  inHYDF$DoY <- seqAcross(inHYDF)
  plot(inHYDF$DoY, inHYDF$Q,"l")
  for(i in seqAcross(inPeaksDF)){
    aber(inPeaksDF$PeakDay[i])
  }
}

ReCutAllHYs <- function(){
  filter(tAllQ, (YMD>=ymd("1911-10-01") & YMD <=ymd("1912-09-30"))) -> HY1912
  filter(tAllQ, (YMD>=ymd("1912-10-01") & YMD <=ymd("1913-09-30"))) -> HY1913
  filter(tAllQ, (YMD>=ymd("1913-10-01") & YMD <=ymd("1914-09-30"))) -> HY1914
  filter(tAllQ, (YMD>=ymd("1914-10-01") & YMD <=ymd("1915-09-30"))) -> HY1915
  filter(tAllQ, (YMD>=ymd("1915-10-01") & YMD <=ymd("1916-09-30"))) -> HY1916
  filter(tAllQ, (YMD>=ymd("1916-10-01") & YMD <=ymd("1917-09-30"))) -> HY1917
  filter(tAllQ, (YMD>=ymd("1917-10-01") & YMD <=ymd("1918-09-30"))) -> HY1918
  filter(tAllQ, (YMD>=ymd("1918-10-01") & YMD <=ymd("1919-09-30"))) -> HY1919
  filter(tAllQ, (YMD>=ymd("1919-10-01") & YMD <=ymd("1920-09-30"))) -> HY1920
  filter(tAllQ, (YMD>=ymd("1920-10-01") & YMD <=ymd("1921-09-30"))) -> HY1921
  filter(tAllQ, (YMD>=ymd("1921-10-01") & YMD <=ymd("1922-09-30"))) -> HY1922
  filter(tAllQ, (YMD>=ymd("1922-10-01") & YMD <=ymd("1923-09-30"))) -> HY1923
  filter(tAllQ, (YMD>=ymd("1923-10-01") & YMD <=ymd("1924-09-30"))) -> HY1924
  filter(tAllQ, (YMD>=ymd("1924-10-01") & YMD <=ymd("1925-09-30"))) -> HY1925
  filter(tAllQ, (YMD>=ymd("1925-10-01") & YMD <=ymd("1926-09-30"))) -> HY1926
  filter(tAllQ, (YMD>=ymd("1926-10-01") & YMD <=ymd("1927-09-30"))) -> HY1927
  filter(tAllQ, (YMD>=ymd("1927-10-01") & YMD <=ymd("1928-09-30"))) -> HY1928
  filter(tAllQ, (YMD>=ymd("1928-10-01") & YMD <=ymd("1929-09-30"))) -> HY1929
  filter(tAllQ, (YMD>=ymd("1929-10-01") & YMD <=ymd("1930-09-30"))) -> HY1930
  filter(tAllQ, (YMD>=ymd("1930-10-01") & YMD <=ymd("1931-09-30"))) -> HY1931
  filter(tAllQ, (YMD>=ymd("1931-10-01") & YMD <=ymd("1932-09-30"))) -> HY1932
  filter(tAllQ, (YMD>=ymd("1932-10-01") & YMD <=ymd("1933-09-30"))) -> HY1933
  filter(tAllQ, (YMD>=ymd("1933-10-01") & YMD <=ymd("1934-09-30"))) -> HY1934
  filter(tAllQ, (YMD>=ymd("1934-10-01") & YMD <=ymd("1935-09-30"))) -> HY1935
  filter(tAllQ, (YMD>=ymd("1935-10-01") & YMD <=ymd("1936-09-30"))) -> HY1936
  filter(tAllQ, (YMD>=ymd("1936-10-01") & YMD <=ymd("1937-09-30"))) -> HY1937
  filter(tAllQ, (YMD>=ymd("1937-10-01") & YMD <=ymd("1938-09-30"))) -> HY1938
  filter(tAllQ, (YMD>=ymd("1938-10-01") & YMD <=ymd("1939-09-30"))) -> HY1939
  filter(tAllQ, (YMD>=ymd("1939-10-01") & YMD <=ymd("1940-09-30"))) -> HY1940
  filter(tAllQ, (YMD>=ymd("1940-10-01") & YMD <=ymd("1941-09-30"))) -> HY1941
  filter(tAllQ, (YMD>=ymd("1941-10-01") & YMD <=ymd("1942-09-30"))) -> HY1942
  filter(tAllQ, (YMD>=ymd("1942-10-01") & YMD <=ymd("1943-09-30"))) -> HY1943
  filter(tAllQ, (YMD>=ymd("1943-10-01") & YMD <=ymd("1944-09-30"))) -> HY1944
  filter(tAllQ, (YMD>=ymd("1944-10-01") & YMD <=ymd("1945-09-30"))) -> HY1945
  filter(tAllQ, (YMD>=ymd("1945-10-01") & YMD <=ymd("1946-09-30"))) -> HY1946
  filter(tAllQ, (YMD>=ymd("1946-10-01") & YMD <=ymd("1947-09-30"))) -> HY1947
  filter(tAllQ, (YMD>=ymd("1947-10-01") & YMD <=ymd("1948-09-30"))) -> HY1948
  filter(tAllQ, (YMD>=ymd("1948-10-01") & YMD <=ymd("1949-09-30"))) -> HY1949
  filter(tAllQ, (YMD>=ymd("1949-10-01") & YMD <=ymd("1950-09-30"))) -> HY1950
  filter(tAllQ, (YMD>=ymd("1950-10-01") & YMD <=ymd("1951-09-30"))) -> HY1951
  filter(tAllQ, (YMD>=ymd("1951-10-01") & YMD <=ymd("1952-09-30"))) -> HY1952
  filter(tAllQ, (YMD>=ymd("1952-10-01") & YMD <=ymd("1953-09-30"))) -> HY1953
  filter(tAllQ, (YMD>=ymd("1953-10-01") & YMD <=ymd("1954-09-30"))) -> HY1954
  filter(tAllQ, (YMD>=ymd("1954-10-01") & YMD <=ymd("1955-09-30"))) -> HY1955
  filter(tAllQ, (YMD>=ymd("1955-10-01") & YMD <=ymd("1956-09-30"))) -> HY1956
  filter(tAllQ, (YMD>=ymd("1956-10-01") & YMD <=ymd("1957-09-30"))) -> HY1957
  filter(tAllQ, (YMD>=ymd("1957-10-01") & YMD <=ymd("1958-09-30"))) -> HY1958
  filter(tAllQ, (YMD>=ymd("1958-10-01") & YMD <=ymd("1959-09-30"))) -> HY1959
  filter(tAllQ, (YMD>=ymd("1959-10-01") & YMD <=ymd("1960-09-30"))) -> HY1960
  filter(tAllQ, (YMD>=ymd("1960-10-01") & YMD <=ymd("1961-09-30"))) -> HY1961
  filter(tAllQ, (YMD>=ymd("1961-10-01") & YMD <=ymd("1962-09-30"))) -> HY1962
  filter(tAllQ, (YMD>=ymd("1962-10-01") & YMD <=ymd("1963-09-30"))) -> HY1963
  filter(tAllQ, (YMD>=ymd("1963-10-01") & YMD <=ymd("1964-09-30"))) -> HY1964
  filter(tAllQ, (YMD>=ymd("1964-10-01") & YMD <=ymd("1965-09-30"))) -> HY1965
  filter(tAllQ, (YMD>=ymd("1965-10-01") & YMD <=ymd("1966-09-30"))) -> HY1966
  filter(tAllQ, (YMD>=ymd("1966-10-01") & YMD <=ymd("1967-09-30"))) -> HY1967
  filter(tAllQ, (YMD>=ymd("1967-10-01") & YMD <=ymd("1968-09-30"))) -> HY1968
  filter(tAllQ, (YMD>=ymd("1968-10-01") & YMD <=ymd("1969-09-30"))) -> HY1969
  filter(tAllQ, (YMD>=ymd("1969-10-01") & YMD <=ymd("1970-09-30"))) -> HY1970
  filter(tAllQ, (YMD>=ymd("1970-10-01") & YMD <=ymd("1971-09-30"))) -> HY1971
  filter(tAllQ, (YMD>=ymd("1971-10-01") & YMD <=ymd("1972-09-30"))) -> HY1972
  filter(tAllQ, (YMD>=ymd("1972-10-01") & YMD <=ymd("1973-09-30"))) -> HY1973
  filter(tAllQ, (YMD>=ymd("1973-10-01") & YMD <=ymd("1974-09-30"))) -> HY1974
  filter(tAllQ, (YMD>=ymd("1974-10-01") & YMD <=ymd("1975-09-30"))) -> HY1975
  filter(tAllQ, (YMD>=ymd("1975-10-01") & YMD <=ymd("1976-09-30"))) -> HY1976
  filter(tAllQ, (YMD>=ymd("1976-10-01") & YMD <=ymd("1977-09-30"))) -> HY1977
  filter(tAllQ, (YMD>=ymd("1977-10-01") & YMD <=ymd("1978-09-30"))) -> HY1978
  filter(tAllQ, (YMD>=ymd("1978-10-01") & YMD <=ymd("1979-09-30"))) -> HY1979
  filter(tAllQ, (YMD>=ymd("1979-10-01") & YMD <=ymd("1980-09-30"))) -> HY1980
  filter(tAllQ, (YMD>=ymd("1980-10-01") & YMD <=ymd("1981-09-30"))) -> HY1981
  filter(tAllQ, (YMD>=ymd("1981-10-01") & YMD <=ymd("1982-09-30"))) -> HY1982
  filter(tAllQ, (YMD>=ymd("1982-10-01") & YMD <=ymd("1983-09-30"))) -> HY1983
  filter(tAllQ, (YMD>=ymd("1983-10-01") & YMD <=ymd("1984-09-30"))) -> HY1984
  filter(tAllQ, (YMD>=ymd("1984-10-01") & YMD <=ymd("1985-09-30"))) -> HY1985
  filter(tAllQ, (YMD>=ymd("1985-10-01") & YMD <=ymd("1986-09-30"))) -> HY1986
  filter(tAllQ, (YMD>=ymd("1986-10-01") & YMD <=ymd("1987-09-30"))) -> HY1987
  filter(tAllQ, (YMD>=ymd("1987-10-01") & YMD <=ymd("1988-09-30"))) -> HY1988
  filter(tAllQ, (YMD>=ymd("1988-10-01") & YMD <=ymd("1989-09-30"))) -> HY1989
  filter(tAllQ, (YMD>=ymd("1989-10-01") & YMD <=ymd("1990-09-30"))) -> HY1990
  filter(tAllQ, (YMD>=ymd("1990-10-01") & YMD <=ymd("1991-09-30"))) -> HY1991
  filter(tAllQ, (YMD>=ymd("1991-10-01") & YMD <=ymd("1992-09-30"))) -> HY1992
  filter(tAllQ, (YMD>=ymd("1992-10-01") & YMD <=ymd("1993-09-30"))) -> HY1993
  filter(tAllQ, (YMD>=ymd("1993-10-01") & YMD <=ymd("1994-09-30"))) -> HY1994
  filter(tAllQ, (YMD>=ymd("1994-10-01") & YMD <=ymd("1995-09-30"))) -> HY1995
  filter(tAllQ, (YMD>=ymd("1995-10-01") & YMD <=ymd("1996-09-30"))) -> HY1996
  filter(tAllQ, (YMD>=ymd("1996-10-01") & YMD <=ymd("1997-09-30"))) -> HY1997
  filter(tAllQ, (YMD>=ymd("1997-10-01") & YMD <=ymd("1998-09-30"))) -> HY1998
  filter(tAllQ, (YMD>=ymd("1998-10-01") & YMD <=ymd("1999-09-30"))) -> HY1999
  filter(tAllQ, (YMD>=ymd("1999-10-01") & YMD <=ymd("2000-09-30"))) -> HY2000
  filter(tAllQ, (YMD>=ymd("2000-10-01") & YMD <=ymd("2001-09-30"))) -> HY2001
  filter(tAllQ, (YMD>=ymd("2001-10-01") & YMD <=ymd("2002-09-30"))) -> HY2002
  filter(tAllQ, (YMD>=ymd("2002-10-01") & YMD <=ymd("2003-09-30"))) -> HY2003
  filter(tAllQ, (YMD>=ymd("2003-10-01") & YMD <=ymd("2004-09-30"))) -> HY2004
  filter(tAllQ, (YMD>=ymd("2004-10-01") & YMD <=ymd("2005-09-30"))) -> HY2005
  filter(tAllQ, (YMD>=ymd("2005-10-01") & YMD <=ymd("2006-09-30"))) -> HY2006
  filter(tAllQ, (YMD>=ymd("2006-10-01") & YMD <=ymd("2007-09-30"))) -> HY2007
  filter(tAllQ, (YMD>=ymd("2007-10-01") & YMD <=ymd("2008-09-30"))) -> HY2008
  filter(tAllQ, (YMD>=ymd("2008-10-01") & YMD <=ymd("2009-09-30"))) -> HY2009
  filter(tAllQ, (YMD>=ymd("2009-10-01") & YMD <=ymd("2010-09-30"))) -> HY2010
  filter(tAllQ, (YMD>=ymd("2010-10-01") & YMD <=ymd("2011-09-30"))) -> HY2011
  filter(tAllQ, (YMD>=ymd("2011-10-01") & YMD <=ymd("2012-09-30"))) -> HY2012
  filter(tAllQ, (YMD>=ymd("2012-10-01") & YMD <=ymd("2013-09-30"))) -> HY2013
  filter(tAllQ, (YMD>=ymd("2013-10-01") & YMD <=ymd("2014-09-30"))) -> HY2014
  filter(tAllQ, (YMD>=ymd("2014-10-01") & YMD <=ymd("2015-09-30"))) -> HY2015
  filter(tAllQ, (YMD>=ymd("2015-10-01") & YMD <=ymd("2016-09-30"))) -> HY2016
  filter(tAllQ, (YMD>=ymd("2016-10-01") & YMD <=ymd("2017-09-30"))) -> HY2017
  filter(tAllQ, (YMD>=ymd("2017-10-01") & YMD <=ymd("2018-09-30"))) -> HY2018
}



# filter(tAllQ, (YMD>=ymd("1911-10-01") & YMD <=ymd("1912-09-30"))) -> HY1912
# filter(tAllQ, (YMD>=ymd("1912-10-01") & YMD <=ymd("1913-09-30"))) -> HY1913
# filter(tAllQ, (YMD>=ymd("1913-10-01") & YMD <=ymd("1914-09-30"))) -> HY1914
# filter(tAllQ, (YMD>=ymd("1914-10-01") & YMD <=ymd("1915-09-30"))) -> HY1915
# filter(tAllQ, (YMD>=ymd("1915-10-01") & YMD <=ymd("1916-09-30"))) -> HY1916
# filter(tAllQ, (YMD>=ymd("1916-10-01") & YMD <=ymd("1917-09-30"))) -> HY1917
# filter(tAllQ, (YMD>=ymd("1917-10-01") & YMD <=ymd("1918-09-30"))) -> HY1918
# filter(tAllQ, (YMD>=ymd("1918-10-01") & YMD <=ymd("1919-09-30"))) -> HY1919
# filter(tAllQ, (YMD>=ymd("1919-10-01") & YMD <=ymd("1920-09-30"))) -> HY1920
# filter(tAllQ, (YMD>=ymd("1920-10-01") & YMD <=ymd("1921-09-30"))) -> HY1921
# filter(tAllQ, (YMD>=ymd("1921-10-01") & YMD <=ymd("1922-09-30"))) -> HY1922
# filter(tAllQ, (YMD>=ymd("1922-10-01") & YMD <=ymd("1923-09-30"))) -> HY1923
# filter(tAllQ, (YMD>=ymd("1923-10-01") & YMD <=ymd("1924-09-30"))) -> HY1924
# filter(tAllQ, (YMD>=ymd("1924-10-01") & YMD <=ymd("1925-09-30"))) -> HY1925
# filter(tAllQ, (YMD>=ymd("1925-10-01") & YMD <=ymd("1926-09-30"))) -> HY1926
# filter(tAllQ, (YMD>=ymd("1926-10-01") & YMD <=ymd("1927-09-30"))) -> HY1927
# filter(tAllQ, (YMD>=ymd("1927-10-01") & YMD <=ymd("1928-09-30"))) -> HY1928
# filter(tAllQ, (YMD>=ymd("1928-10-01") & YMD <=ymd("1929-09-30"))) -> HY1929
# filter(tAllQ, (YMD>=ymd("1929-10-01") & YMD <=ymd("1930-09-30"))) -> HY1930
# filter(tAllQ, (YMD>=ymd("1930-10-01") & YMD <=ymd("1931-09-30"))) -> HY1931
# filter(tAllQ, (YMD>=ymd("1931-10-01") & YMD <=ymd("1932-09-30"))) -> HY1932
# filter(tAllQ, (YMD>=ymd("1932-10-01") & YMD <=ymd("1933-09-30"))) -> HY1933
# filter(tAllQ, (YMD>=ymd("1933-10-01") & YMD <=ymd("1934-09-30"))) -> HY1934
# filter(tAllQ, (YMD>=ymd("1934-10-01") & YMD <=ymd("1935-09-30"))) -> HY1935
# filter(tAllQ, (YMD>=ymd("1935-10-01") & YMD <=ymd("1936-09-30"))) -> HY1936
# filter(tAllQ, (YMD>=ymd("1936-10-01") & YMD <=ymd("1937-09-30"))) -> HY1937
# filter(tAllQ, (YMD>=ymd("1937-10-01") & YMD <=ymd("1938-09-30"))) -> HY1938
# filter(tAllQ, (YMD>=ymd("1938-10-01") & YMD <=ymd("1939-09-30"))) -> HY1939
# filter(tAllQ, (YMD>=ymd("1939-10-01") & YMD <=ymd("1940-09-30"))) -> HY1940
# filter(tAllQ, (YMD>=ymd("1940-10-01") & YMD <=ymd("1941-09-30"))) -> HY1941
# filter(tAllQ, (YMD>=ymd("1941-10-01") & YMD <=ymd("1942-09-30"))) -> HY1942
# filter(tAllQ, (YMD>=ymd("1942-10-01") & YMD <=ymd("1943-09-30"))) -> HY1943
# filter(tAllQ, (YMD>=ymd("1943-10-01") & YMD <=ymd("1944-09-30"))) -> HY1944
# filter(tAllQ, (YMD>=ymd("1944-10-01") & YMD <=ymd("1945-09-30"))) -> HY1945
# filter(tAllQ, (YMD>=ymd("1945-10-01") & YMD <=ymd("1946-09-30"))) -> HY1946
# filter(tAllQ, (YMD>=ymd("1946-10-01") & YMD <=ymd("1947-09-30"))) -> HY1947
# filter(tAllQ, (YMD>=ymd("1947-10-01") & YMD <=ymd("1948-09-30"))) -> HY1948
# filter(tAllQ, (YMD>=ymd("1948-10-01") & YMD <=ymd("1949-09-30"))) -> HY1949
# filter(tAllQ, (YMD>=ymd("1949-10-01") & YMD <=ymd("1950-09-30"))) -> HY1950
# filter(tAllQ, (YMD>=ymd("1950-10-01") & YMD <=ymd("1951-09-30"))) -> HY1951
# filter(tAllQ, (YMD>=ymd("1951-10-01") & YMD <=ymd("1952-09-30"))) -> HY1952
# filter(tAllQ, (YMD>=ymd("1952-10-01") & YMD <=ymd("1953-09-30"))) -> HY1953
# filter(tAllQ, (YMD>=ymd("1953-10-01") & YMD <=ymd("1954-09-30"))) -> HY1954
# filter(tAllQ, (YMD>=ymd("1954-10-01") & YMD <=ymd("1955-09-30"))) -> HY1955
# filter(tAllQ, (YMD>=ymd("1955-10-01") & YMD <=ymd("1956-09-30"))) -> HY1956
# filter(tAllQ, (YMD>=ymd("1956-10-01") & YMD <=ymd("1957-09-30"))) -> HY1957
# filter(tAllQ, (YMD>=ymd("1957-10-01") & YMD <=ymd("1958-09-30"))) -> HY1958
# filter(tAllQ, (YMD>=ymd("1958-10-01") & YMD <=ymd("1959-09-30"))) -> HY1959
# filter(tAllQ, (YMD>=ymd("1959-10-01") & YMD <=ymd("1960-09-30"))) -> HY1960
# filter(tAllQ, (YMD>=ymd("1960-10-01") & YMD <=ymd("1961-09-30"))) -> HY1961
# filter(tAllQ, (YMD>=ymd("1961-10-01") & YMD <=ymd("1962-09-30"))) -> HY1962
# filter(tAllQ, (YMD>=ymd("1962-10-01") & YMD <=ymd("1963-09-30"))) -> HY1963
# filter(tAllQ, (YMD>=ymd("1963-10-01") & YMD <=ymd("1964-09-30"))) -> HY1964
# filter(tAllQ, (YMD>=ymd("1964-10-01") & YMD <=ymd("1965-09-30"))) -> HY1965
# filter(tAllQ, (YMD>=ymd("1965-10-01") & YMD <=ymd("1966-09-30"))) -> HY1966
# filter(tAllQ, (YMD>=ymd("1966-10-01") & YMD <=ymd("1967-09-30"))) -> HY1967
# filter(tAllQ, (YMD>=ymd("1967-10-01") & YMD <=ymd("1968-09-30"))) -> HY1968
# filter(tAllQ, (YMD>=ymd("1968-10-01") & YMD <=ymd("1969-09-30"))) -> HY1969
# filter(tAllQ, (YMD>=ymd("1969-10-01") & YMD <=ymd("1970-09-30"))) -> HY1970
# filter(tAllQ, (YMD>=ymd("1970-10-01") & YMD <=ymd("1971-09-30"))) -> HY1971
# filter(tAllQ, (YMD>=ymd("1971-10-01") & YMD <=ymd("1972-09-30"))) -> HY1972
# filter(tAllQ, (YMD>=ymd("1972-10-01") & YMD <=ymd("1973-09-30"))) -> HY1973
# filter(tAllQ, (YMD>=ymd("1973-10-01") & YMD <=ymd("1974-09-30"))) -> HY1974
# filter(tAllQ, (YMD>=ymd("1974-10-01") & YMD <=ymd("1975-09-30"))) -> HY1975
# filter(tAllQ, (YMD>=ymd("1975-10-01") & YMD <=ymd("1976-09-30"))) -> HY1976
# filter(tAllQ, (YMD>=ymd("1976-10-01") & YMD <=ymd("1977-09-30"))) -> HY1977
# filter(tAllQ, (YMD>=ymd("1977-10-01") & YMD <=ymd("1978-09-30"))) -> HY1978
# filter(tAllQ, (YMD>=ymd("1978-10-01") & YMD <=ymd("1979-09-30"))) -> HY1979
# filter(tAllQ, (YMD>=ymd("1979-10-01") & YMD <=ymd("1980-09-30"))) -> HY1980
# filter(tAllQ, (YMD>=ymd("1980-10-01") & YMD <=ymd("1981-09-30"))) -> HY1981
# filter(tAllQ, (YMD>=ymd("1981-10-01") & YMD <=ymd("1982-09-30"))) -> HY1982
# filter(tAllQ, (YMD>=ymd("1982-10-01") & YMD <=ymd("1983-09-30"))) -> HY1983
# filter(tAllQ, (YMD>=ymd("1983-10-01") & YMD <=ymd("1984-09-30"))) -> HY1984
# filter(tAllQ, (YMD>=ymd("1984-10-01") & YMD <=ymd("1985-09-30"))) -> HY1985
# filter(tAllQ, (YMD>=ymd("1985-10-01") & YMD <=ymd("1986-09-30"))) -> HY1986
# filter(tAllQ, (YMD>=ymd("1986-10-01") & YMD <=ymd("1987-09-30"))) -> HY1987
# filter(tAllQ, (YMD>=ymd("1987-10-01") & YMD <=ymd("1988-09-30"))) -> HY1988
# filter(tAllQ, (YMD>=ymd("1988-10-01") & YMD <=ymd("1989-09-30"))) -> HY1989
# filter(tAllQ, (YMD>=ymd("1989-10-01") & YMD <=ymd("1990-09-30"))) -> HY1990
# filter(tAllQ, (YMD>=ymd("1990-10-01") & YMD <=ymd("1991-09-30"))) -> HY1991
# filter(tAllQ, (YMD>=ymd("1991-10-01") & YMD <=ymd("1992-09-30"))) -> HY1992
# filter(tAllQ, (YMD>=ymd("1992-10-01") & YMD <=ymd("1993-09-30"))) -> HY1993
# filter(tAllQ, (YMD>=ymd("1993-10-01") & YMD <=ymd("1994-09-30"))) -> HY1994
# filter(tAllQ, (YMD>=ymd("1994-10-01") & YMD <=ymd("1995-09-30"))) -> HY1995
# filter(tAllQ, (YMD>=ymd("1995-10-01") & YMD <=ymd("1996-09-30"))) -> HY1996
# filter(tAllQ, (YMD>=ymd("1996-10-01") & YMD <=ymd("1997-09-30"))) -> HY1997
# filter(tAllQ, (YMD>=ymd("1997-10-01") & YMD <=ymd("1998-09-30"))) -> HY1998
# filter(tAllQ, (YMD>=ymd("1998-10-01") & YMD <=ymd("1999-09-30"))) -> HY1999
# filter(tAllQ, (YMD>=ymd("1999-10-01") & YMD <=ymd("2000-09-30"))) -> HY2000
# filter(tAllQ, (YMD>=ymd("2000-10-01") & YMD <=ymd("2001-09-30"))) -> HY2001
# filter(tAllQ, (YMD>=ymd("2001-10-01") & YMD <=ymd("2002-09-30"))) -> HY2002
# filter(tAllQ, (YMD>=ymd("2002-10-01") & YMD <=ymd("2003-09-30"))) -> HY2003
# filter(tAllQ, (YMD>=ymd("2003-10-01") & YMD <=ymd("2004-09-30"))) -> HY2004
# filter(tAllQ, (YMD>=ymd("2004-10-01") & YMD <=ymd("2005-09-30"))) -> HY2005
# filter(tAllQ, (YMD>=ymd("2005-10-01") & YMD <=ymd("2006-09-30"))) -> HY2006
# filter(tAllQ, (YMD>=ymd("2006-10-01") & YMD <=ymd("2007-09-30"))) -> HY2007
# filter(tAllQ, (YMD>=ymd("2007-10-01") & YMD <=ymd("2008-09-30"))) -> HY2008
# filter(tAllQ, (YMD>=ymd("2008-10-01") & YMD <=ymd("2009-09-30"))) -> HY2009
# filter(tAllQ, (YMD>=ymd("2009-10-01") & YMD <=ymd("2010-09-30"))) -> HY2010
# filter(tAllQ, (YMD>=ymd("2010-10-01") & YMD <=ymd("2011-09-30"))) -> HY2011
# filter(tAllQ, (YMD>=ymd("2011-10-01") & YMD <=ymd("2012-09-30"))) -> HY2012
# filter(tAllQ, (YMD>=ymd("2012-10-01") & YMD <=ymd("2013-09-30"))) -> HY2013
# filter(tAllQ, (YMD>=ymd("2013-10-01") & YMD <=ymd("2014-09-30"))) -> HY2014
# filter(tAllQ, (YMD>=ymd("2014-10-01") & YMD <=ymd("2015-09-30"))) -> HY2015
# filter(tAllQ, (YMD>=ymd("2015-10-01") & YMD <=ymd("2016-09-30"))) -> HY2016
# filter(tAllQ, (YMD>=ymd("2016-10-01") & YMD <=ymd("2017-09-30"))) -> HY2017
# filter(tAllQ, (YMD>=ymd("2017-10-01") & YMD <=ymd("2018-09-30"))) -> HY2018
