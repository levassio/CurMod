no_cores <- detectCores()
cl <- makeCluster(no_cores)
system.time(parLapply(cl, X = rep(1000000, 320), sieve))
stopCluster(cl)

system.time(foreach(nn=rep(1000000, 1)) %do% sieve(nn))




sieve <- function(n)
{
  n <- as.integer(n)
  
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  for(i in last.prime:floor(sqrt(n)))
  {
    primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
    last.prime <- last.prime + min(which(primes[(last.prime+1):n]))
  }
  which(primes)
}