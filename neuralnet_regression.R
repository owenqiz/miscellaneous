#Question 2
# part (a)
set.seed(1)
n <- 60
x <- runif(n, min = -2, max = 2)
err <- rnorm(n)
y <- x^3 + err

# part (b)
activlogistic <- function(x){1/(1+exp(-x))}
deractivlogistic <- function(x){exp(-x)/(1+exp(-x))^2}

# part (c)
p <- 1
M <- 5
K <- 1

nneteval <- function(xinvec, wmat1, wmat2, activfunc){
  xMatrix <- rbind(1,xinvec)
  a1 <- wmat1 %*% xMatrix
  z1 <- activfunc(a1)
  zAugmented <- rbind(1,z1)
  youtvec <- c(wmat2 %*% zAugmented)
  return(youtvec)
}

nnetSSE <- function(yinvec, xinvec, wmat1, wmat2, activfun){
  yPred <- nneteval(xinvec, wmat1, wmat2, activfun)
  sse <- sum((yinvec-yPred)^2)
  return(sse)
}

# part (d)
nnetGradient <- function(yinvec, xinvec, wmat1, wmat2, activfunc, deractivfunc){
  # since for regression the activation function is identity, deractivfunc is 1
  yHat <- nneteval(xinvec, wmat1, wmat2, activfunc)
  
  delta <- -2*(yinvec-yHat)
  
  a1 <- wmat1 %*% rbind(1,xinvec)
  
  # wmat2 (n x (M+1)), where n is observation, each row is cbind(1,z1)
  # matrix multiplication deal with summation among samples
  wmat2Gradient <- t(delta) %*% t(rbind(1, activfunc(a1))) 
  
  # multiply by one more delta1 between layers ,i.e. delta from a2 to a1, deractivfunc(a1)
  # matrix multiplication deal with summation among samples
  # wmat2 drop 1st element because no link between this neuron to wmat1
  wmat1Gradient <- (outer(wmat2[-1],delta) * deractivfunc(a1)) %*% cbind(1,xinvec)

  GradientList <- list(wmat1Gradient,wmat2Gradient)
  return(GradientList)
}

# part (e)
learningRate <- .01
iteration <- 200

set.seed(1)
wmat1 <- matrix(rnorm(M*(p+1)),nrow = M, ncol = (p+1))
wmat2 <- matrix(rnorm(K*(M+1)),nrow = K, ncol = (M+1))

for(j in 1:iteration){
  for(i in 1:n){
    
    W <- nnetGradient(y[i],x[i],wmat1,wmat2,activlogistic,deractivlogistic)
    
    wmat1 <- wmat1 - learningRate * W[[1]]
    wmat2 <- wmat2 - learningRate * W[[2]]
  }
}

# part(f)
# wmat1, wmat2 from part (e)
yhat <- nneteval(x,wmat1,wmat2,activlogistic)

ggplot(data = data.frame(x,y,yhat), aes(x = x)) + geom_point(aes(y = y, colour = "Observation")) +
       geom_point(aes(y = yhat, colour = "Prediction")) + ggtitle("Neural Net Prediction") +
       scale_color_manual(values = c("black","blue")) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="top", legend.title = element_blank())