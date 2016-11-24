funOmitNA = function(x) {
  drop = FALSE
  n = ncol(x)
  for (i in 1:n)
    drop = drop | is.na(x[, i])
  x[!drop, ]
}

reportProgress <- function(current, total, ..., step = 1, startTime = NA){
  
  if(total >= 10000){
    
    one <- total %/% (100 / step)
    reminder <- (current - 1) %% one
    
    #  print(reminder)
    #  print(": reminder")
    
    if (reminder == 0){
      
      if(current > 1 & !is.na(startTime)){
        
        currentTime <- Sys.time()
        secPerOne <- difftime(currentTime, startTime, units = "secs") / current
        eta <- currentTime + (total - current) * secPerOne
        
        print(paste(format(Sys.time(), "%H:%M:%OS3"), paste(round(((current - 1) / one) * step), "%", sep = ""), "secPerMil:", round(secPerOne * 1000000, digits = 2), "eta:", format(eta, "%H:%M:%S"),  paste(...)))
      }
      else{
        
        print(paste(format(Sys.time(), "%H:%M:%OS3"), paste(round(((current - 1) / one) * step), "%", sep = ""), paste(...))) 
        
      }
    }
  } else {
    currentTime <- Sys.time()
    secPerOne <- difftime(currentTime, startTime, units = "secs") / current
    eta <- currentTime + (total - current) * secPerOne
    
    print(paste(format(Sys.time(), "%H:%M:%OS3"), paste(current, "/", total, sep = ""), "secPerMil:", round(secPerOne * 1000000, digits = 2), "eta:", format(eta, "%H:%M:%S"),  paste(...)))
  }
}

shiftForward <- function(x, n = 1){
  l <- length(x)
  res <- numeric(l)
  res[1:n] <- NA
  res[(n + 1):l] <- x[1:(l-n)]
  res
}

getTypical <- function(x){
  (x$Open + x$High + x$Low + x$Close) / 4
}

intDateRange <- function(x, rangeStart, rangeEnd = NA){
  
  if(is.na(rangeEnd)){
    rangeEnd <- rangeStart
  }
  
  if(rangeStart == rangeEnd){
     rangeEnd <- rangeStart + 1
  }
  
  rangeStart <- rangeStart * (10 ^ (9 - nchar(as.character(rangeStart))))
  rangeEnd <- rangeEnd * (10 ^ (9 - nchar(as.character(rangeEnd)))) - 1
  
  library("data.table")
  setDT(x)
  setkey(x, Date)
  
  res <- x[Date >= rangeStart & Date <= rangeEnd]
  
  print(head(res$Date, 1))
  print(tail(res$Date, 1))
  
  res
}

finish <- function(x){
  print.size(x)
  #  setDT(x)
  x
}

print.size <- function(x){
  print(format(object.size(x), units = "Mb"))
}

prepareTrainingSet <- function(x, tradeColumn, testSample = .2){
  
  tradeColumn <- as.factor(tradeColumn)
  
  x$Date <- NULL
  x$Open <- NULL
  x$High <- NULL
  x$Low <- NULL
  x$Close <- NULL
  x$Volume <- NULL
  
  x[, grepl("trade.*", names(x))] <- NULL
  x[, grepl("life.*", names(x))] <- NULL
  
  x$Trade <- tradeColumn
  
  x <- funOmitNA(x)
  
  l <- nrow(x)
  
  testSampleStart <- l - (l * testSample)
  testSample <- testSampleStart:l
  
  testX <- x[testSample,]
  trainX <- x[-testSample,]
  
  res <- list(trainX, testX)
  res
}

prepareTrainingSetByCols <- function(x, tradeColumn, importantColumns){
  tradeColumn <- as.factor(tradeColumn)
  
  x <- x[, importantColumns]
  x$Trade <- tradeColumn
  x
}

sampleDF <- function(x, size){
  x[sample(nrow(x), size), ]
}