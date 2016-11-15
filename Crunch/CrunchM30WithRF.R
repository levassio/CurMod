library(randomForest)
#trainingSet <- EURUSD_m5_Trades_1000_40_20
trainingSet <- EURUSDm30_trade_150_1000_10

nVector <- seq(8, 70, 1)

trainingSet <- appendAboveSMAs(trainingSet, nVector)
trainingSet <- appendVolatility(trainingSet, nVector)
trainingSet <- appendMAAngles(trainingSet, nVector)
trainingSet <- adjustByVolatility(trainingSet, nVector, newAdjusteeName = "aboveSMAAD")

trainingSetList <- prepareTrainingSet(trainingSet, trainingSet$trade.580, testSample = 0.2)

#test speed
#trainingSetList <- prepareTrainingSet(trainingSet, trainingSet$trade.580, testSample = .3, last = TRUE)
#print(Sys.time())
#rfModel <- randomForest(Trade ~ ., data = trainingSetList[[2]], ntree = 100, replace = FALSE, importance = TRUE)
#print(Sys.time())
#test speed

system.time(rfModel <- randomForest(Trade ~ ., data = trainingSetList[[1]], ntree = 50, importance = TRUE, sampsize = 5000, nodesize = 100))
print(rfModel)

imp <- importance(rfModel)
imp <- data.table(cname = rownames(imp), imp)
setorder(imp, -MeanDecreaseAccuracy)
importantColumns <- head(imp$cname, 126)

actual <- trainingSetList[[2]]$Trade
predicted <- predict(rfModel, newdata = trainingSetList[[2]])
predictionTable <- table(actual, predicted)

buyRatio <- predictionTable[4]/(predictionTable[4] + predictionTable[3])
selRatio <- predictionTable[1]/(predictionTable[1] + predictionTable[2])

print("run 1")
predictionTable
buyRatio
selRatio

importantColumns <- c("volatil.290", "sma.angle.290", "volatil.90", "sma.angle.90")
importantColumns <- c(
  "volatil.100", "sma.angle.100", "aboveSMAAD.100", "aboveSMA.100",
  "volatil.34", "sma.angle.34", "aboveSMAAD.34", "aboveSMA.34",
  "volatil.8", "sma.angle.8", "aboveSMAAD.8", "aboveSMA.8")

#start

trainingSet2 <- prepareTrainingSetByCols(trainingSet, trainingSet$trade.580, importantColumns)
trainingSetList2 <- prepareTrainingSet(trainingSet2, trainingSet2$Trade, testSample = 0.2)
system.time(rfModel2 <- randomForest(Trade ~ ., data = trainingSetList2[[1]], ntree = 100, importance = TRUE, sampsize = 10000, nodesize = 28))
print(rfModel2)

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

print("run 2")
predictionTable2
selRatio2
buyRatio2
buyDiff2

#end

imp2 <- importance(rfModel2)
imp2 <- data.table(cname = rownames(imp2), imp2)
setorder(imp2, -MeanDecreaseAccuracy)
importantColumns2 <- head(imp2$cname, 45)

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
