appendVolatility <- function(x, periods, lifeColumnPrefix = "life", avgFutureStepPrefix = "avgFutureStep"){
  
  library("data.table")
  setDF(x)
  
  totalRows <- nrow(x)
  totalWork <- totalRows * length(periods)
  current <- 1
  startTime <- Sys.time()
  
  Typical1 <- x$Open + x$Close + x$High + x$Low
  Typical2 <- x$Close + x$High + x$Low
  Typical3 <- x$Volume
  Typical4 <- x$Volume
  
  for(period in periods){
    
    avgFutureStep1 <- numeric(totalRows)
    avgFutureStep2 <- numeric(totalRows)
    avgFutureStep3 <- numeric(totalRows)
    avgFutureStep4 <- numeric(totalRows)
    
    lifeColumn <- x[, paste(lifeColumnPrefix, period, sep = ".")]
    
    for(i in 1:totalRows){
      sum1 <- 0
      sum2 <- 0
      sum3 <- 0
      sum4 <- 0
      
      prevClose1 <- Typical1[i]
      prevClose2 <- Typical2[i]
      prevClose3 <- Typical3[i]
      prevClose4 <- Typical4[i]
      
      sum4 <- prevClose4
      
      life <- lifeColumn[i]
      
      if(!is.na(life) & life > 0){
        
        #avgFutureStep
        for(j in (i+1):(i + life)){
          
          currClose1 <- Typical1[j]
          currClose2 <- Typical2[j]
          currClose3 <- Typical3[j]
          currClose4 <- Typical4[j]
          
          sum1 <- sum1 + abs(currClose1 - prevClose1)
          sum2 <- sum2 + abs(currClose2 - prevClose2)
          sum3 <- sum3 + abs(currClose3 - prevClose3)
          sum4 <- sum4 + currClose4
          
          prevClose1 <- currClose1
          prevClose2 <- currClose2
          prevClose3 <- currClose3
          prevClose4 <- currClose4
        }
        
        avgFutureStep1[i] <- sum1
        avgFutureStep2[i] <- sum2
        avgFutureStep3[i] <- sum3
        avgFutureStep4[i] <- sum4
      }
      
      reportProgress(current, totalWork, startTime = startTime, step = 1)
      current <- current + 1
    }
    
    avgFutureStepColName1 <- paste("avgFutureStep1", period, sep = ".")
    avgFutureStepColName2 <- paste("avgFutureStep2", period, sep = ".")
    avgFutureStepColName3 <- paste("avgFutureStep3", period, sep = ".")
    avgFutureStepColName4 <- paste("avgFutureStep4", period, sep = ".")
    
    x[,avgFutureStepColName1] <- avgFutureStep1
    x[,avgFutureStepColName2] <- avgFutureStep2
    x[,avgFutureStepColName3] <- avgFutureStep3
    x[,avgFutureStepColName4] <- avgFutureStep4
  }
  
  setDT(x)
  x
}