lst <- list(
  matrix(1:4, nrow = 2),
  matrix(5:8, nrow = 2),
  matrix(9:12, nrow = 2),
  matrix(13:16, nrow = 2)
)

library(purrr)

# using purrr style function
lst %>% map( ~.x[,1])
# map(lst, ~ .x[,1] )