
allReturn <- function(p = 25, n){
  # see excel sheet for two-stage formula
  r <- .025 # for testing purpose
  # p <- 25 # for testing perpose
  
  r1 <- p*r
  accr1 <- 0
  fl1 <- floor(r1+accr1)
  rem1 <- r1 - fl1
  
    if (n == 1)
    return(p+rem1)
  else{
    pvec <- rvec <- accrvec <- flvec <- remvec <- rep(NA,n-1)
    pvec <- c(p,pvec)
    rvec <- c(r1,rvec)
    accrvec <- c(accr1,accrvec)
    flvec <- c(fl1,flvec)
    remvec <- c(rem1,remvec)
    for(i in 2:n){
      pvec[i] <- pvec[i-1]+flvec[i-1]
      rvec[i] <- pvec[i]*r
      accrvec[i] <- rvec[i]+remvec[i-1]
      flvec[i] <- floor(accrvec[i])
      remvec[i] <- accrvec[i] - flvec[i]
    }
    return(pvec[n]+remvec[n]+flvec[n])
  }
}

  t <- seq(1,100) 
  tsq <- t^2

  r10 <- sapply(t, allReturn, p = 10)
  r25 <- sapply(t, allReturn, p = 25)
  
  df <- data.frame(cbind(t = (1:100), tsq = (1:100)^2, r10,r25))
  
  reg25 <- lm(r25 ~ t + tsq, data = df) 
  summary(reg25)
  pr25 <- predict(reg25, list(t=df$t, tsq=df$tsq))
  reg10 <- lm(r10 ~ t + tsq, data = df) 
  summary(reg10)
  pr10 <- predict(reg10, list(t=df$t, tsq=df$tsq))
  
  df <- data.frame(df,pr10,pr25)
  
  ggplot(data = df, aes(x = t)) + geom_point(aes(y = r10, colour = "papa")) + geom_line(aes(y = pr10, colour = "papa")) +
    geom_point(aes(y = r25, colour = "mama")) + geom_line(aes(y = pr25, colour = "mama")) + 
    scale_color_manual(values = c("black","blue")) + xlab("month") + ylab("return")
  