makeTrainingSet <- function(x, nVector = seq(2,100,2)){
  
  trainingSet <- x
  
  trainingSet <- appendAboveSMAs(trainingSet, nVector)
  trainingSet <- appendVolatility(trainingSet, nVector)
  trainingSet <- appendMAAngles(trainingSet, nVector)
  trainingSet <- adjustByVolatility(trainingSet, nVector, newAdjusteeName = "aboveSMAAD")
  #  trainingSet <- adjustByVolatility(trainingSet, nVector)
  
  trainingSet
}

crunchTunes <- function(trainingSet, tunesPopulation){
  
  smpSize <- nrow(tunesPopulation)
  print(paste("smpSize", smpSize))
  
  library(foreach)
  library(doSNOW)
  library(iterators)
  
  cores <- detectCores(logical = FALSE)
  cl <- makePSOCKcluster(cores, outfile = "")
  registerDoSNOW(cl)
  
  clusterExport(cl, c("crunchRF", "prepareTrainingSet", "prepareTrainingSetByCols", "funOmitNA", "reportProgress"))
  
  startTime <- Sys.time()
  res <- foreach(ind = iter(1:nrow(tunesPopulation)), .packages = c("randomForest")) %dopar% crunchRF(ind, tunesPopulation, trainingSet, startTime)
    
  for(j in res) {
    tunesPopulation[tunesPopulation$index == j[1], 4:6] <- j[2:4]
  }
  
  stopCluster(cl)
  tunesPopulation
}

crunchRF <- function(ind, tunesPopulation, trainingSet, startTime){
  
  cs <- tunesPopulation[ind,]
  
  trainingSetList <- prepareTrainingSet(trainingSet, trainingSet[,paste("trade", cs[1], sep = ".")], testSample = 0.2)
  trainData <- trainingSetList[[1]]
  testData <- trainingSetList[[2]]
  
  rfModel <- randomForest(Trade ~ ., data = trainData, ntree = 50, sampsize = as.integer(cs[2]), nodesize = as.integer(cs[3]))
  
  predicted <- predict(rfModel, newdata = testData)
  predictionTable <- table(testData$Trade, predicted)
  buyRatio <- predictionTable[4]/(predictionTable[4] + predictionTable[3])
  selRatio <- predictionTable[1]/(predictionTable[1] + predictionTable[2])
  
  runCount <- as.integer(cs[6])
  prevB <- ifelse(is.na(cs[4]), 0, as.double(cs[4]) * runCount)
  prevS <- ifelse(is.na(cs[5]), 0, as.double(cs[5]) * runCount)
  runCount <- runCount + 1
  
  newCS <- c(cs[7], (buyRatio + prevB)/runCount, (selRatio + prevS)/runCount, runCount)
  reportProgress(ind, nrow(tunesPopulation), startTime = startTime)
  
  newCS
}

mergeResultSetsNTunes <- function(df1, df2){
  
  bound <- rbind(df1, df2)
  bound$buyScore <- bound$buyScore * bound$runCount
  bound$selScore <- bound$selScore * bound$runCount
  
  library(data.table)
  setDT(bound)
  
  res <- bound[, .(buyScore = sum(buyScore), selScore = sum(selScore), runCount = sum(runCount)), by =.(nVector)]
  
  res$buyScore <- res$buyScore / res$runCount
  res$selScore <- res$selScore / res$runCount
  
  setDF(res)
  
  res[,c("nVector", "buyScore", "selScore", "runCount")]
}

mergeResultSets <- function(df1, df2){
  
  bound <- rbind(df1, df2)
  bound$buyScore <- bound$buyScore * bound$runCount
  bound$selScore <- bound$selScore * bound$runCount
  
  library(data.table)
  setDT(bound)
  
  res <- bound[, .(buyScore = sum(buyScore), selScore = sum(selScore), runCount = sum(runCount)), by =.(trade, sampSize, minNode, index)]
  
  res$buyScore <- res$buyScore / res$runCount
  res$selScore <- res$selScore / res$runCount
  
  setDF(res)
  
  res[,c("trade", "sampSize", "minNode", "buyScore", "selScore", "runCount", "index")]
}

reducer <- function(x, tunes, times){
  
  x <- makeTrainingSet(x)
  
  library(data.table)
  setDT(tunes)
  n <- nrow(tunes)
  
  startTime <- Sys.time()
  current <- 1
  while(current <= times){
    
    smp <-  as.integer(c(sample(1:24, 6), sample(24:n, 6)))
    
    setDT(tunes)
    tunes <- tunes[order(-buyScore)]
    crunchSettings <- tunes[smp,]
    
    print(head(tunes))
    
    crunchSettings$buyScore <- NA
    crunchSettings$selScore <- NA
    crunchSettings$runCount <- 0
    
    crunchSettings <- crunchSettings[order(-sampSize)]
    crunchSettings <- crunchTunes(x, crunchSettings)
    
    tunes <- mergeResultSets(crunchSettings, tunes)
    
    reportProgress(current, times, startTime = startTime)
    current <- current + 1
  }
  
  print(startTime)
  print(Sys.time())
  tunes
}

featureTuner <- function(tset, nTunes, times, isLowToHigh = FALSE){
  
  setDT(nTunes)
  nTunes <- nTunes[order(-buyScore)]
  
  current <- 1
  overallStartTime <- Sys.time()
  
  library(parallel)
  library(foreach)
  library(doSNOW)
  library(iterators)
  
  cores <- detectCores()
  cl <- makePSOCKcluster(cores, outfile = "")
  registerDoSNOW(cl)
  
  
  
  clusterExport(cl, c("crunchRFNVec", "prepareTrainingSet", "prepareTrainingSetByCols", "funOmitNA", "reportProgress"))
  
  while(current <= times){
    
    smp <-  as.integer(c(sample(1:24, 12), sample(25:nrow(nTunes), 12)))
    
    print(head(nTunes))
    
    crunchSettings <- nTunes[smp,]
    setDF(crunchSettings)
    
    crunchSettings$buyScore <- NA
    crunchSettings$selScore <- NA
    crunchSettings$runCount <- 0
    
    print("sample:")
    print(smp)
    
    workerStartTime <- Sys.time()
    res <- foreach(ind = iter(1:nrow(crunchSettings)), .packages = c("randomForest")) %dopar% crunchRFNVec(ind, crunchSettings, tset, workerStartTime, isLowToHigh)
    
    for(j in res) {
      crunchSettings[crunchSettings$nVector == j[1], 2:4] <- j[2:4]
    }
    
    nTunes <- mergeResultSetsNTunes(nTunes, crunchSettings)
    
    reportProgress(current, times, overallStartTime)
    current <- current + 1
    
    setDT(nTunes)
    nTunes <- nTunes[order(-buyScore)]
  }
  
  stopCluster(cl)
  
  print(overallStartTime)
  print(Sys.time())
  
  nTunes
}

addUniqueKeyToTunes <- function(t){
  t$index <- t$sampSize + t$minNode + t$trade * 10000
  t
}

crunchRFNVec <- function(ind, nTunes, trainingSet, startTime, isLowToHigh = FALSE){
  
  curTune <- nTunes[ind,]
  indexed <- as.integer(curTune[1])
  
  if(isLowToHigh){
    
    n <- integer(4)
    n[1] <- indexed %/% 1000000
    indexed <- indexed - n[1] * 1000000
    n[2] <- indexed %/% 10000
    indexed <- indexed - n[2] * 10000
    n[3] <- indexed %/% 100
    indexed <- indexed - n[3] * 100
    n[4] <- indexed
    indexed <- as.integer(curTune[1])
    
    remainCols <- character(4*4 + 1)
    remainCols[1] <- "trade.380"
    
    exgrid <- expand.grid(c("volatil", "sma.angle", "aboveSMAAD", "aboveSMA"), n)
    remainCols[2:17] <- paste(exgrid[,1], exgrid[,2], sep = ".")
    
    trainingSet <- trainingSet[, remainCols]
  }
  else{
    drops <- paste(c("volatil", "sma.angle", "aboveSMAAD", "aboveSMA"), indexed, sep = ".")
    trainingSet <- trainingSet[, !(names(trainingSet) %in% drops)]  
  }
  
  trainingSetList <- prepareTrainingSet(trainingSet, trainingSet[,paste("trade", 380, sep = ".")], testSample = 0.2)
  trainData <- trainingSetList[[1]]
  testData <- trainingSetList[[2]]
  
  rfModel <- randomForest(Trade ~ ., data = trainData, ntree = 50, sampsize = 47000, nodesize = 50)
  
  predicted <- predict(rfModel, newdata = testData)
  predictionTable <- table(testData$Trade, predicted)
  buyRatio <- predictionTable[4]/(predictionTable[4] + predictionTable[3])
  selRatio <- predictionTable[1]/(predictionTable[1] + predictionTable[2])
  
  runCount <- as.integer(curTune[4])
  prevB <- ifelse(is.na(curTune[2]), 0, as.double(curTune[2]) * runCount)
  prevS <- ifelse(is.na(curTune[3]), 0, as.double(curTune[3]) * runCount)
  runCount <- runCount + 1
  
  newCS <- c(indexed, (buyRatio + prevB)/runCount, (selRatio + prevS)/runCount, runCount)
  reportProgress(ind, nrow(nTunes), startTime = startTime, indexed)
  
  newCS
}
