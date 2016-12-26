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

#reduce features from 16

predictors <- decodeColNames(2798597)
predictorsLength <- length(predictors)
vec <- 1:predictorsLength

encoded1 <- encodePeriods(vec, 1)
encoded2 <- encodePeriods(vec, 2)
encoded3 <- encodePeriods(vec, 3)

smp <- c(encoded1, encoded2, encoded3)

nTunesDropOne <- data.frame(
  nVector <- smp,
  buyScore <- double(length(smp)),
  selScore <- double(length(smp)),
  runcount <- 0
)

colnames(nTunesDropOne) <- c("nVector", "buyScore", "selScore", "runCount")

nTunesDropOne <- featureTuner(tset, nTunesDropOne, 30, "D1")
x <- featureTuner(tset, x, 30, "D1")

#retune RF settings
tunesPopulation <- expand.grid(
  nTree = c(15, 20, 25),
  nodesize = seq(1, 99, 2),
  sampsize = seq(10, 99, 2)
)

encodedTunes <- tunesPopulation$nTree * 10000 + tunesPopulation$nodesize * 100 + tunesPopulation$sampsize

resRetuneRF <- data.frame(
  encodedTunes,
  buyScore <- double(length(encodedTunes)),
  selScore <- double(length(encodedTunes)),
  runcount <- 0
)
colnames(resRetuneRF) <- c("nVector", "buyScore", "selScore", "runCount")

initialLength <- nrow(resRetuneRF)

for(i in 1:5){
  print(i)
  resRetuneRF <- featureTuner(tset, resRetuneRF, 1, "TF")
  setDT(resRetuneRF)
  resRetuneRF <- resRetuneRF[runCount < 2 | totalScore > 1.1]
  resRetuneRF <- resRetuneRF[runCount < 4 | totalScore > 1.11]
  resRetuneRF <- resRetuneRF[runCount < 8 | totalScore > 1.122]
}

print(paste("dropped", initialLength - nrow(resRetuneRF)))
