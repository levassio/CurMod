library(readr)
EURUSD_Minute30 <- read_csv("~/Dropbox/Projects/DATA/HistData/EURUSD_Minute30.csv", col_names = FALSE)
colnames(EURUSD_Minute30) <- c("Date", "Open", "High", "Low", "Close", "Volume")
EURUSD_Minute30_380 <- appendTrades(EURUSD_Minute30, 380)

bestPeriods <- 2798597
drops <- 12
rfTunes <- 257136
rfTunes <- 259944

rfTunes <- decodeColNames(rfTunes, withNames = FALSE)
ntree <- rfTunes[1] * 10
sampsize <- rfTunes[2]
nodesize <- rfTunes[3]

predictorPeriods <- decodeColNames(bestPeriods, withNames = FALSE)
predictorColNames <- decodeColNames(bestPeriods)
tset <- makeTrainingSet(EURUSD_Minute30_380, predictorPeriods)

dropIndexes <- decodeColNames(drops, withNames = FALSE)
dropNames <- predictorColNames[dropIndexes]

tset <- tset[, !names(tset) %in% dropNames]
trainingSetList <- prepareTrainingSet(tset, tset[,paste("trade", 380, sep = ".")], testSample = 0.022)
trainData <- trainingSetList[[1]]
testData <- trainingSetList[[2]]

rfModel <- randomForest(Trade ~ ., data = trainData, ntree = ntree, sampsize = (sampsize / 100) * nrow(trainData), nodesize = nodesize)

predicted <- predict(rfModel, newdata = testData)
predictionTable <- table(testData$Trade, predicted)
buyRatio <- predictionTable[4]/(predictionTable[4] + predictionTable[3])
selRatio <- predictionTable[1]/(predictionTable[1] + predictionTable[2])

print(paste("buyRatio:", buyRatio))
print(paste("selRatio:", selRatio))

#deploy