appendRSI_SMA <- function(df, nVector, colName = "rsi_sma"){
  library("TTR")
  
  for(n in nVector){
    
    rsi <- RSI(df$Open, n, maType = "SMA")
    indName <- paste(colName, n, sep = ".")
    
    df[,indName] <- rsi
  }
  
  df
}

appendRSI_SMA_MAN <- function(df, nVector, colName = "rsi_sma_man"){
  library("TTR")
  
  for(n in nVector){
    
    rsi <- RSI_man(df$Open, n)
    indName <- paste(colName, n, sep = ".")
    
    df[,indName] <- rsi
  }
  
  df
}

appendRSI_SMA_VOL <- function(df, nVector, colName = "rsi_sma_vol"){
  library("TTR")
  
  current <- 1
  total <- length(nVector)
  startTime <- Sys.time()
  
  for(n in nVector){
    
    reportProgress(current, total, startTime = startTime)
    current <- current + 1
    
    rsi <- RSI_vol_man(df$Open, df$Volume, n)
    indName <- paste(colName, n, sep = ".")
    
    df[,indName] <- rsi
  }
  
  df
}

appendRSI_EMA <- function(df, nVector, colName = "rsi_ema"){
  library("TTR")
  
  for(n in nVector){
    
    rsi <- RSI(df$Open, n, maType = "EMA")
    indName <- paste(colName, n, sep = ".")
    
    df[,indName] <- rsi
  }
  
  df
}

RSI_man <- function(vec, n) {
  RSI_vol_man(vec, 1, n);
}

RSI_vol_man <- function(vec, volume, n){
  
  shiftedVec <- numeric(length(vec))
  shiftedVec <- c(NA, vec[1:(length(vec) - 1)])
  changes <- (vec - shiftedVec) * volume
  
  res <- numeric(length(vec))
  
  i <- 1
  sumGains <- 0
  sumLosses <- 0
  prev <- NA
  
  for(curChange in changes) {
    
    if(i > 1){
      if(curChange > 0){
        sumGains <- sumGains + curChange
      }
      else if(curChange < 0){
        sumLosses <- sumLosses + curChange
      }
    }
    
    if(i > n)
    {
      
      if((avgLosses <- abs(sumLosses/n)) <= 0) {
        res[i] <- 100
      } else if((avgGains <- sumGains/n) <= 0) {
        res[i] <- 0
      } else {
        res[i] <- 100 - (100/(1 + avgGains/avgLosses))
      }
      
      oldChange <- changes[i-n+1]
      
      if(oldChange > 0){
        sumGains <- sumGains - oldChange
      }
      else if(oldChange < 0){
        sumLosses <- sumLosses - oldChange
      }
    }
    else {
      res[i] <- NA
    }
    
    i <- i+1 
  }
  
  res
}
