adjustYbyX <- function(y, x){   #y sma, x vol
  
  
  #probably different mean and regression should be used for positives and negatives
  
  df <- data.frame(x, y)
  colnames(df) <- c("x", "y")
  df <- funOmitNA(df)
  
  yAbs <- abs(df$y)
  yMeanSign <- sign(df$y) * mean(yAbs)
  df$y <- yAbs
  
  totalLength <- length(y)
  naLength <- totalLength - nrow(df)
  
  lregr <- lm(df$y ~ df$x)
  
  expectedY <- predict.lm(lregr, data.frame(df$x), na.action = na.omit)
  
  
  adjusted <- double(totalLength)
  
  adjusted[1:naLength] <- NA
  adjusted[(naLength+1):totalLength] <- yMeanSign * df$y/expectedY
  adjusted
}