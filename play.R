tunesPopulation <- expand.grid(
  trade = seq(150, 1000, 10),
  sampSize = seq(3000, 50000, 500),
  minNode = seq(5, 100, 1),
  buyScore = NA,
  selScore = NA,
  runCount = 0
)

tunesPopulation$index <- 1:nrow(tunesPopulation)

print.size(tunesPopulation)

tunesSample <- sampleDF(tunesPopulation, 20)

tuned <- crunchTunes(EURUSDm30_trade_150_1000_10, tunesSample, progressSteps = 5)

