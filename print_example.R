m <- matrix(1:16, ncol=4)

print(m)

print.mat <- function(x) {
  n <- nrow(x)
  for(i in seq_len(n)) {
    cat(paste("This is row", i, "\t: " ))
    cat(x[i,], "\n")
  }
}

class(m) <- c("mat", class(m))

print(m)


# references
# https://stackoverflow.com/questions/10938427/example-needed-change-the-default-print-method-of-an-object
# https://stackoverflow.com/questions/52036040/r-how-to-keep-print-method-for-custom-class
methods(print)
?methods