makeTrainingSet <- function(x, nVector = seq(2,100,1)){
  
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
  
  res$buyScore <- ifelse(res$runCount > 0, res$buyScore / res$runCount, 0)
  res$selScore <- ifelse(res$runCount > 0, res$selScore / res$runCount, 0)
  
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

featureTuner <- function(tset, nTunes, times, mode = "TF"){
  
  nTunes$totalScore <- nTunes$buyScore + nTunes$selScore
  
  setDT(nTunes)
  nTunes <- nTunes[order(-totalScore)]
  
  current <- 1
  overallStartTime <- Sys.time()
  
  library(parallel)
  library(foreach)
  library(doSNOW)
  library(iterators)
  
  cores <- detectCores()
  cl <- makePSOCKcluster(cores, outfile = "")
  registerDoSNOW(cl)
  
  clusterExport(cl, c("crunchRFNVec", "prepareTrainingSet", "prepareTrainingSetByCols", "funOmitNA", "reportProgress", "decodeColNames"))
  
  while(current <= times){
    
    smpVector <- 1:nrow(nTunes)
    smpProbVector <- smpVector ^ (-.5) * .9
    
    smp <-  as.integer(sample(smpVector, min(c(96, nrow(nTunes))), prob = smpProbVector))
    
    print(head(nTunes, 24))
    
    crunchSettings <- nTunes[smp,]
    setDF(crunchSettings)
    
    crunchSettings$buyScore <- NA
    crunchSettings$selScore <- NA
    crunchSettings$runCount <- 0
    
    print("sample:")
    print(smp)
    
    workerStartTime <- Sys.time()
    res <- foreach(ind = iter(1:nrow(crunchSettings)), .packages = c("randomForest")) %dopar% crunchRFNVec(ind, crunchSettings, tset, workerStartTime, mode)
    
    for(j in res) {
      crunchSettings[crunchSettings$nVector == j[1], 2:4] <- j[2:4]
    }
    
    nTunes <- mergeResultSetsNTunes(nTunes, crunchSettings)
    
    reportProgress(current, times, overallStartTime)
    current <- current + 1
    
    nTunes$totalScore <- nTunes$buyScore + nTunes$selScore
    
    setDT(nTunes)
    nTunes <- nTunes[order(-totalScore)]
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

encodePeriods <- function(nVector, take = 4) {
  
  cs <- data.frame(t(combn(nVector, take)))
  res <- integer(nrow(cs))
  
  for (t in 1:take){
    res <- res + as.integer(cs[,t]) * 10 ^ ((take - t) * 2)
  }
  
  if(FALSE){
    cs$ind <- cs$X1 * 1000000 + cs$X2 * 10000 + cs$X3 * 100 + cs$X4
    as.integer(cs$ind)
  }
  
  res
}

decodeColNames <- function(encoded, withNames = TRUE){
  #change to pass in names vector if need names
  
  periods <- ifelse(encoded == 1, 1, ceiling(log10(encoded) / 2))
  n <- integer(periods)
  
  for(period in 1:periods) {
    zeros <- 10 ^ ((periods - period) * 2)
    n[period] <- encoded %/% zeros
    encoded <- encoded - n[period] * zeros
  }
  
  if(withNames){
    exgrid <- expand.grid(c("volatil", "sma.angle", "aboveSMAAD", "aboveSMA"), n)
    result <- paste(exgrid[,1], exgrid[,2], sep = ".")
    result
  } else {
    n
  }
}

crunchRFNVec <- function(ind, nTunes, trainingSet, startTime, mode = "TF"){
  
  #browser()
  
  curTune <- nTunes[ind,]
  encodedRunSettings <- as.integer(curTune[1])
  
  #defaults
  
  rfTunes <- 208450   #ntree, sampsize, nodesize
  bestPeriods <- 2798597
  dropColumns <- 12
  
  switch(mode,
         I = {
           # INCREMENT BY PERIOD
           bestPeriods <- encodedRunSettings
         },
         TF = {
           #TUNE FOREST
           rfTunes <- encodedRunSettings
         },
         D1 = {
           # DROP ONE BY ONE
           
           dropColumns = encodedRunSettings
         })
  
  predictorCols <- decodeColNames(bestPeriods)
  dropColumns = decodeColNames(dropColumns, withNames = FALSE)
  predictorCols <- predictorCols[-dropColumns]
  colsLength <- length(predictorCols) + 1
  
  remainCols <- character(colsLength)
  remainCols[1] <- "trade.380"
  remainCols[2:colsLength] <- predictorCols
  
  trainingSet <- trainingSet[, remainCols]
  
  rfTunes <- decodeColNames(rfTunes, withNames = FALSE)
  ntree <- rfTunes[1] * 10
  sampsize <- rfTunes[2]
  nodesize <- rfTunes[3]
  
  trainingSetList <- prepareTrainingSet(trainingSet, trainingSet[,paste("trade", 380, sep = ".")], testSample = 0.2)
  trainData <- trainingSetList[[1]]
  testData <- trainingSetList[[2]]
  
  rfModel <- randomForest(Trade ~ ., data = trainData, ntree = ntree, sampsize = (sampsize / 100) * nrow(trainData), nodesize = nodesize)
  
  predicted <- predict(rfModel, newdata = testData)
  predictionTable <- table(testData$Trade, predicted)
  buyRatio <- predictionTable[4]/(predictionTable[4] + predictionTable[3])
  selRatio <- predictionTable[1]/(predictionTable[1] + predictionTable[2])
  
  runCount <- as.integer(curTune[4])
  prevB <- ifelse(is.na(curTune[2]), 0, as.double(curTune[2]) * runCount)
  prevS <- ifelse(is.na(curTune[3]), 0, as.double(curTune[3]) * runCount)
  runCount <- runCount + 1
  
  newCS <- c(encodedRunSettings, (buyRatio + prevB)/runCount, (selRatio + prevS)/runCount, runCount)
  reportProgress(ind, nrow(nTunes), startTime = startTime, encodedRunSettings)
  
  newCS
}
