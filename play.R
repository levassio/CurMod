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

encoded <- encodePeriods(nVector, 4)
smp <- sample(encoded, 10000)

nTunesLowToHigh <- data.frame(
  nVector <- smp,
  buyScore <- double(length(smp)),
  selScore <- double(length(smp)),
  runcount <- 0
)

colnames(nTunesLowToHigh) <- c("nVector", "buyScore", "selScore", "runCount")

nTunesLowToHigh <- featureTuner(tset, nTunesLowToHigh, 240, TRUE)





