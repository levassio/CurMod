runAnything <- function(n){
  
  startTime <- Sys.time()
  
  runnee <- function(){
    
    system.time(rfModel2 <- randomForest(Trade ~ ., data = trainingSetList2[[1]], ntree = 100, importance = TRUE, sampsize = 10000, nodesize = 28))
    #print(rfModel2)
    
    actual2 <- trainingSetList2[[2]]$Trade
    predicted2 <- predict(rfModel2, newdata = trainingSetList2[[2]])
    predictionTable2 <- table(actual2, predicted2)
    buyRatio2 <- predictionTable2[4]/(predictionTable2[4] + predictionTable2[3])
    selRatio2 <- predictionTable2[1]/(predictionTable2[1] + predictionTable2[2])
    
    lp2 <- length(predicted2)
    randomPredicted2 <- as.factor(ifelse(runif(lp2) > .5, 580, 580))
    randomPredictionTable2 <- table(actual2, randomPredicted2)
    randomBuyRatio2 <- randomPredictionTable2[2]/(randomPredictionTable2[2] + randomPredictionTable2[1])
    buyDiff2 <- buyRatio2 - randomBuyRatio2
    
    bd2 <- c(bd2, buyDiff2)
    
    print("run 2")
    predictionTable2
    selRatio2
    buyRatio2
    buyDiff2
    
  }
  
  library(foreach)
  library(doParallel)
  
  cores <- detectCores()
  registerDoParallel(cores)
  
  res <- foreach(1:n, .combine = c) %dopar% runnee()
  
  stopImplicitCluster()
  print(difftime(Sys.time(), startTime))
  print(mean(res))
  plot(density(res))
  res
}