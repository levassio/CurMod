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

tunesPopulation$index <- 1:nrow(tunesPopulation)

print.size(tunesPopulation)

tunesSample <- sampleDF(tunesPopulation, 40)

tuned <- crunchTunes(EURUSDm30_trade_150_1000_10, tunesSample)

#iRich crunches 40 50tree forests in 10 min