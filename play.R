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

#reduce features
nVector <- seq(2,99,1)
tset <- makeTrainingSet(EURUSDm30_trade_150_1000_10)

nTunes <- data.frame(
  nVector <- nVector,
  buyScore <- double(length(nVector)),
  selScore <- double(length(nVector)),
  runcount <- 0
)

colnames(nTunes) <- c("nVector", "buyScore", "selScore", "runCount")


nTunes <- calcNTunes(tset, nTunes)
print(1)

nTunesTmp <- featureReducer(tset, nTunes, 2)



