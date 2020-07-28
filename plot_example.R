plotm <- function(m){
  # pause between plots 
  par(ask = TRUE)
  
  hist(m)
  plot(density(m))
  image(m)
}

plotm(matrix(rnorm(100), ncol = 10))

