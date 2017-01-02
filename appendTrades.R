appendTrades <- function(x, stops) {
  
  print(format(object.size(x), units = "Mb"))
  
  current <- 1
  totalRows <- nrow(x)
  totalTrades <- nrow(x) * length(stops)
  startTime <- Sys.time()
  
  stops <- stops / 100000
  
  Open <- x$Open
  RowMean <- with(x, (Low + High) / 2)
  RowIndex <- as.numeric(rownames(x))
  
  s <- 0
  for (stop in stops) {
    s <- s+1
    
    lossLimitVec <- with(x, pmax((Low + stop), RowMean))
    profitLimitVec <- with(x, pmin((High - stop), RowMean))
    
    stopTicks <- round(stop * 100000)
    
    closeColumn <- numeric(totalRows) + NA
    tradeColumn <- numeric(totalRows) + stopTicks
    
    negativeTicks <- stopTicks * -1
    
    print(paste(Sys.time(), "stop:", stopTicks))
    
    i <- 1
    
    jtot <- 0
    lastEntry <- Open[1]
    
    counter <- 1
    changeTot <- 0
    
    for(entryPrice in Open) {
      
      j <- i
      
      while(j <= totalRows){
        
        if(entryPrice >= lossLimitVec[j]) {
          tradeColumn[i] <- negativeTicks
          closeColumn[i] <- j
          break
        } else if(entryPrice <= profitLimitVec[j]) {
          closeColumn[i] <- j
          break
        }
        
        j <- j+1
      }
      
      if(j > totalRows){
        tradeColumn[i] <- NA
      }
      
      changeTot <- changeTot + abs(entryPrice - lastEntry)
      jtot <- jtot + (j-i)
      
      reportProgress(current, totalTrades, step = 1, startTime = startTime, i, jtot/counter, changeTot / counter)
      current <- current + 1
      counter <- counter + 1
      
      i <- i + 1
      
      if(i %% 40000 == 0){
        jtot <- 0
        changeTot <- 0
        counter <- 0
      }
      
      lastEntry <- entryPrice
    }
    
    tradeColumnName <- paste("trade", stopTicks, sep = ".")
    lifeColumnName <- paste("life", stopTicks, sep = ".")
    
    x[, tradeColumnName] <- tradeColumn
    x[, lifeColumnName] <- closeColumn - RowIndex
  }
  
  library("data.table")
  setDT(x)
  print(format(object.size(x), units = "Mb"))
  x
  
}