crunchTunes <- function(x, tunesPopulation, nVector = seq(2,100,2)){
  
  startTime <- Sys.time()
  smpSize <- nrow(tunesPopulation)
  print(paste("smpSize", smpSize))
  
  library(foreach)
  library(doSNOW)
#  library(doParallel)
  library(iterators)
  
  trainingSet <- x
  
  trainingSet <- appendAboveSMAs(trainingSet, nVector)
  trainingSet <- appendVolatility(trainingSet, nVector)
  trainingSet <- appendMAAngles(trainingSet, nVector)
  trainingSet <- adjustByVolatility(trainingSet, nVector, newAdjusteeName = "aboveSMAAD")
#  trainingSet <- adjustByVolatility(trainingSet, nVector)
  
  cores <- detectCores()
  cl <- makePSOCKcluster(cores, outfile = "")
  registerDoSNOW(cl)
#  registerDoParallel(cl)
  
  clusterExport(cl, c("crunchRF", "prepareTrainingSet", "prepareTrainingSetByCols", "funOmitNA"))
    
  res <- foreach(ind = iter(1:nrow(tunesPopulation)), .packages = c("randomForest")) %dopar% crunchRF(ind, tunesPopulation, trainingSet)
    
  for(j in res) {
    tunesPopulation[tunesPopulation$index == j[1], 4:6] <- j[2:4]
  }
  
  stopCluster(cl)
  
  tunesPopulation
}

crunchRF <- function(ind, tunesPopulation, trainingSet){
  
  print(ind)
  cs <- tunesPopulation[ind,]
  
  trainingSetList <- prepareTrainingSet(trainingSet, trainingSet[,paste("trade", cs[1], sep = ".")], testSample = 0.2)
  trainData <- trainingSetList[[1]]
  testData <- trainingSetList[[2]]
  
  rfModel <- randomForest(Trade ~ ., data = trainData, ntree = 10, sampsize = as.integer(cs[2]), nodesize = as.integer(cs[3]))
  
  predicted <- predict(rfModel, newdata = testData)
  predictionTable <- table(testData$Trade, predicted)
  buyRatio <- predictionTable[4]/(predictionTable[4] + predictionTable[3])
  selRatio <- predictionTable[1]/(predictionTable[1] + predictionTable[2])
  
  runCount <- as.integer(cs[6])
  prevB <- ifelse(is.na(cs[4]), 0, as.double(cs[4]) * runCount)
  prevS <- ifelse(is.na(cs[5]), 0, as.double(cs[5]) * runCount)
  runCount <- runCount + 1
  
  newCS <- c(cs[7], (buyRatio + prevB)/runCount, (selRatio + prevS), runCount)
  newCS
}