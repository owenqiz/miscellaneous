m <- matrix(rnorm(300), nrow = 3)
x <- 1:3

f1 <- function(m, x){
  for(i in 1:nrow(m))
    m[i,] <- m[i,] - x[i]
  
  return(sum(m^2))
}

f2 <- function(m, x){
  n_col <- ncol(m)
  x <- matrix(rep(x, n_col), ncol = n_col)
  return(sum((m-x)^2))
}

f3 <- function(m, x){
  m <- t(sweep(t(m), 2, x, '-'))
  return(sum(m^2))
}

f4 <- function(m, x){
  return(sum(sweep(m, 1, x, '-', check.margin = F)^2))
}

library(microbenchmark)
(f <- microbenchmark(f1(m,x), f2(m,x), f3(m,x), f4(m,x), times = 1000))
ggplot2::autoplot(f)


