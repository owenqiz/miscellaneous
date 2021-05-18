p <- c(.75, .15, .1)

mix1 <- function(n, prob){
  x <- rep(NA, n)
  for(i in 1:n){
    u <- runif(1)
    if (u < prob[1])
      x[i] <- rt(1, 4)
    else if (u < prob[2])
      x[i] <- rexp(1, 1/2)
    else
      x[i] <- runif(1, -3, 3)
  }
  return(x)
}

x <- mix1(5000, p)

mix2 <- function(n, prob){
  x <- rep(NA, n)
  idx <- sample(1:3, size = n, prob = prob, replace = TRUE)
  x[idx == 1] <- rt(length(x[idx == 1]), 4)
  x[idx == 2] <- rexp(length(x[idx == 2]), 2)
  x[idx == 3] <- runif(length(x[idx == 3]), -3, 3)
  return(x)
}

ggplot2::autoplot(microbenchmark::microbenchmark(mix1(n,p), mix2(n,p), times = 300))
