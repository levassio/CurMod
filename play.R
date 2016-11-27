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


t4 <- reducer(EURUSDm30_trade_150_1000_10, t4, 10)

#reduce features
nVector <- seq(2,100,2)
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
nTunes <- calcNTunes(tset, nTunes)
print(2)
nTunes <- calcNTunes(tset, nTunes)
print(3)
nTunes <- calcNTunes(tset, nTunes)
print(4)
nTunes <- calcNTunes(tset, nTunes)
print(5)
nTunes <- calcNTunes(tset, nTunes)
print(6)
nTunes <- calcNTunes(tset, nTunes)
print(7)
nTunes <- calcNTunes(tset, nTunes)
print(8)
nTunes <- calcNTunes(tset, nTunes)
print(9)
nTunes <- calcNTunes(tset, nTunes)
print(10)
nTunes <- calcNTunes(tset, nTunes)
print(11)
nTunes <- calcNTunes(tset, nTunes)
print(12)
nTunes <- calcNTunes(tset, nTunes)
print(13)
nTunes <- calcNTunes(tset, nTunes)
print(14)
nTunes <- calcNTunes(tset, nTunes)
print(15)
nTunes <- calcNTunes(tset, nTunes)
print(16)
nTunes <- calcNTunes(tset, nTunes)
print(17)
nTunes <- calcNTunes(tset, nTunes)
print(18)


