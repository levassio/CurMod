library(TTR)
library(data.table)
library(RcppRoll)

appendAboveEMAs <- function(x, nVector, name = "aboveEMA"){
  
  appendAboveMA(x, nVector, EMA, name)
}

appendAboveSMAs <- function(x, nVector, name = "aboveSMA"){
  
  appendAboveMA(x, nVector, SMA, name)
}

appendAboveMA <- function(x, nVector, maFunc, name){
  
  setDF(x)
  
  lengthOfNVector <- length(nVector)
  print(paste("adding", lengthOfNVector, name))

  for(n in nVector){
    
    ma <- maFunc(x$Open, n)
    appendAboveMA <- round((x$Open - ma) * 100000, 1)
    
    aboveMA_name <- paste(name, n, sep = ".")
    x[,aboveMA_name] <- appendAboveMA

#      ma_name <- paste("ma", n, sep = ".")
#      x[,ma_name] <- ma

  }
  
  finish(x)
}

adjustByVolatility <- function(x, nVector, adjusteeName = "aboveSMA", volatilityName = "volatil", newAdjusteeName = adjusteeName){
  for(n in nVector){
    
    aName <- paste(adjusteeName, n, sep = ".")
    vName <- paste(volatilityName, n, sep = ".")
    
    adjusted <- adjustYbyX(x[,aName], x[,vName])
    
    newName <- paste(newAdjusteeName, n, sep = ".")
    x[,newName] <- adjusted
  }
  
  finish(x)
}

appendVolatility <- function(x, nVector, name = "volatil"){
  typical <- getTypical(x)
  shifted <- shiftForward(typical)
  absDiff <- round(abs(typical - shifted) * 100000, 1)
  absDiff[1] <- 0
  l <- nrow(x)
  vecLength <- length(nVector)
  
#  browser()
  setDF(x)
  
  print(paste("adding", vecLength, name))
  
  for(n in nVector){

#doesn't work as fast as advertised - try to use zoo's rollapply(x, 3, sum) or     
#    rsum.cumsum <- function(x, n = 3L) {
#      tail(cumsum(x) - cumsum(c(rep(0, n), head(x, -n))), -n + 1)
      
      
    rs <- roll_sum(absDiff, n)  
    colName <- paste(name, n, sep = ".")
    x[n:l, colName] <- rs
  }
  
  print(paste("added", vecLength, name))
  finish(x)
}

appendMAAngles <- function(x, nVector, maFunc = SMA, maPrefix = "sma", name = "angle"){
  
#  browser()
  
  setDF(x)
  for(n in nVector){
    
    ma <- maFunc(x$Open, n)
    diff <- ma - shiftForward(ma)
    
    colName <- paste(maPrefix, name, n, sep = ".")
    x[,colName] <- diff
    
  }
  
  finish(x)
}

finish <- function(x){
  print(format(object.size(x), units = "Mb"))
#  setDT(x)
  x
}