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
    
    smp <-  as.integer(c(runif(6, 1, 24), runif(6, 24, 500)))
    
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

featureReducer <- function(tset, tunes, times){
  
  current <- 1
  startTime <- Sys.time()
  while(current <= times){
    
    smp <-  as.integer(c(runif(6, 1, 12), runif(6, 13, 50)))
    
    setDT(nTunes)
    nTunes <- nTunes[order(-buyScore)]
    crunchSettings <- nTunes
    setDF(nTunes)
    
    print(head(crunchSettings))
    
    crunchSettings$buyScore <- NA
    crunchSettings$selScore <- NA
    crunchSettings$runCount <- 0
    
    nTunes <- calcNTunes(tset, crunchSettings)
    
    reportProgress(current, times, startTime)
    current <- current + 1
  }
  
}

addUniqueKeyToTunes <- function(t){
  t$index <- t$sampSize + t$minNode + t$trade * 10000
  t
}

calcNTunes <- function(trainingSet, nTunes){
  
  smpSize <- nrow(nTunes)
  print(paste("smpSize", smpSize))
  
  library(parallel)
  library(foreach)
  library(doSNOW)
  library(iterators)
  
  cores <- detectCores(logical = TRUE)
  cl <- makePSOCKcluster(cores, outfile = "")
  registerDoSNOW(cl)
  
  clusterExport(cl, c("crunchRFNVec", "prepareTrainingSet", "prepareTrainingSetByCols", "funOmitNA", "reportProgress"))
  
  startTime <- Sys.time()
  res <- foreach(ind = iter(1:nrow(nTunes)), .packages = c("randomForest")) %dopar% crunchRFNVec(ind, nTunes, trainingSet, startTime)
  
  stopCluster(cl)
  
  for(j in res) {
    nTunes[nTunes$nVector == j[1], 2:4] <- j[2:4]
  }
  
  print(startTime)
  print(Sys.time())
  
  nTunes
}

crunchRFNVec <- function(ind, nTunes, trainingSet, startTime){
  
  curTune <- nTunes[ind,]
  
  drops <- paste(c("volatil", "sma.angle", "aboveSMAAD", "aboveSMA"), curTune[1], sep = ".")
  
  trainingSet <- trainingSet[, !(names(trainingSet) %in% drops)]
  
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
  
  newCS <- c(curTune[1], (buyRatio + prevB)/runCount, (selRatio + prevS)/runCount, runCount)
  reportProgress(ind, nrow(nTunes), startTime = startTime)
  
  newCS
}
