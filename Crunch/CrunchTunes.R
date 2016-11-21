crunchTunes <- function(x, tunesPopulation, nVector = seq(2,100,2), progressSteps = 20){
  
  startTime <- Sys.time()
  smpSize <- nrow(tunesPopulation)/progressSteps
  print(paste("smpSize", smpSize))
  
  library(foreach)
  library(doParallel)
  
  trainingSet <- x
  
  trainingSet <- appendAboveSMAs(trainingSet, nVector)
  trainingSet <- appendVolatility(trainingSet, nVector)
  trainingSet <- appendMAAngles(trainingSet, nVector)
  trainingSet <- adjustByVolatility(trainingSet, nVector, newAdjusteeName = "aboveSMAAD")
#  trainingSet <- adjustByVolatility(trainingSet, nVector)
  
  cores <- detectCores()  
  registerDoParallel(cores)
  indexes <- tunesPopulation$index
  
  for(i in 1:progressSteps){
    smpStart <- 1*i
    smpEnd <- smpStart + smpSize
    
    smp <- tunesPopulation[smpStart:smpEnd,]
    
    res <- foreach(ind = 1:smpSize) %do% crunchRF(ind, smp, trainingSet)
    
    for(j in res) {
      tunesPopulation[tunesPopulation$index == j[1], 4:6] <- j[2:4]
    }
    
    reportProgress(i, progressSteps, startTime = startTime)
  }
  
  stopImplicitCluster()
  
  tunesPopulation
}

crunchRF <- function(ind, smp, trainingSet){
  
  cs <- smp[ind,]
  
  trainingSetList <- prepareTrainingSet(trainingSet, trainingSet[,paste("trade", cs[1], sep = ".")], testSample = 0.2)
  trainData <- trainingSetList[[1]]
  testData <- trainingSetList[[2]]
  
  library(randomForest)
  rfModel <- randomForest(Trade ~ ., data = trainData, ntree = 100, sampsize = as.integer(cs[2]), nodesize = as.integer(cs[3]))
  
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