con <- url("https://dl.dropboxusercontent.com/u/30352637/TrainingData/EURUSDm30_trade_150_1000_10.rda")
load(con)

tunesPopulation <- expand.grid(
  trade = seq(150, 1000, 10),
  sampSize = seq(3000, 50000, 500),
  minNode = seq(5, 100, 1),
  buyScore = NA,
  selScore = NA,
  runCount = 0
)

tunesPopulation <- expand.grid(
  trade = seq(340, 560, 10),
  sampSize = seq(13000, 48500, 500),
  minNode = seq(14, 99, 1),
  buyScore = NA,
  selScore = NA,
  runCount = 0
)


tunesPopulation <- addUniqueKeyToTunes(tunesPopulation)


tTemp <- reducer(EURUSDm30_trade_150_1000_10, t4, 2)

#expand features
nVector <- seq(2,99,1)
tset <- makeTrainingSet(EURUSDm30_trade_150_1000_10, nVector)

nVectorFrom3 <- seq(3,99,1)
encoded2 <- encodePeriods(nVectorFrom3, 2)
encoded3 <- encodePeriods(nVectorFrom3, 3)
encoded4 <- encodePeriods(nVectorFrom3, 4)

encoded2 <- encoded2 + 20000
encoded3 <- encoded3 + 2000000
encoded4 <- encoded4 + 200000000

smp <- c(encoded2, sample(encoded3, 100000), sample(encoded4, 300000))

nTunesLowToHigh345 <- data.frame(
  nVector <- smp,
  buyScore <- double(length(smp)),
  selScore <- double(length(smp)),
  runcount <- 0
)

colnames(nTunesLowToHigh345) <- c("nVector", "buyScore", "selScore", "runCount")

nTunesLowToHigh345 <- featureTuner(tset, nTunesLowToHigh345, 2, "I")

#reduce features

predictors <- decodeColNames(2798597)
predictorsLength <- length(predictors)
vec <- 1:predictorsLength

encoded1 <- encodePeriods(vec, 1)
encoded2 <- encodePeriods(vec, 2)
encoded3 <- encodePeriods(vec, 3)

smp <- c(encoded1, encoded2, encoded3)

nTunesDropOnea <- data.frame(
  nVector <- smp,
  buyScore <- double(length(smp)),
  selScore <- double(length(smp)),
  runcount <- 0
)

colnames(nTunesDropOnea) <- c("nVector", "buyScore", "selScore", "runCount")

nTunesDropOnea <- featureTuner(tset, nTunesDropOnea, 72, "D1")

#pass a runner function to the featureTuner
#runner function should accept decoder function which should decode column names and cut the training set accordingly
#make sure this setup can work both for singlte feature reduction and for 345 feature increment

