crunch <- function(x, tradeVec, nVec, nTreeVec, sampVec, nodeVec){
  
  library(foreach)
  library(doParallel)
  
  trainingSet <- x
  
  trainingSet <- appendAboveSMAs(trainingSet, nVec)
  trainingSet <- appendVolatility(trainingSet, nVec)
  trainingSet <- appendMAAngles(trainingSet, nVec)
  trainingSet <- adjustByVolatility(trainingSet, nVec, newAdjusteeName = "aboveSMAAD")
  
  tunes <- 5
  
  totalLength <- length(tradeVec) * length(nVec) * length(nTreeVec) * length(sampVec) * length(nodeVec)
  
  params <- expand.grid(nVec = nVec, nTreeVec = nTreeVec, sampVec = sampVec, nodeVec = nodeVec)
  
  no_cores <- detectCores()
  print(paste(no_cores, "cores, total:", totalLength))
  
  cores <- detectCores()
  registerDoParallel(cores)
  
  
  
  for(trade in tradeVec){
    tradecol <- paste("trade", trade, sep = "")
    trainingSetList <- prepareTrainingSet(trainingSet, trainingSet[,tradecol], testSample = 0.2)
    
    res <- matrix(parRapply(cl, params, crunchTrees, trainingSetList, tunes, sieve), ncol = tunes, byrow = TRUE)
  }

  
  stopCluster(cl)
  
  print(nrow(res))
}

crunchTrees <- function(params, x, tunes, sieve){
 
  print(params)
  rfModel <- randomForest(Trade ~ ., data = x[[1]], ntree = params[2], importance = TRUE, sampsize = params[3], nodesize = params[4])
  
  params
  
}



