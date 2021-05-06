Mi <- c(50,100,150,200, 500)
psi <- Mi/sum(Mi)
ti <- c(6, 10, 18, 26, 40)
tpsi <- ti/psi
s <- 1:5

# unique sample wr of 2 find their sum, prob, var etc
tm <- outer(tpsi, tpsi, '+')
sp <- outer(s,s, 'paste', sep = ',')
pm <- outer(psi, psi)
sp <- gdata::upperTriangle(sp, diag = TRUE, byrow = TRUE)
pm <- 2 * pm
diag(pm) <- diag(pm)/2
pm <- gdata::upperTriangle(pm, diag = TRUE, byrow = TRUE)
tm <- gdata::upperTriangle(tm, diag = TRUE, byrow = TRUE)/2
tmu <- sum(tm*pm)
idx <- outer(s, s, 'paste0')
idx <- gdata::upperTriangle(idx, diag = TRUE, byrow = TRUE)
idx <- as.numeric(idx)
idx1 <- floor(idx/10)
idx2 <- idx %% 10
tpsi[idx1]
tpsi[idx2]
tpsim <- cbind(tpsi[idx1], tpsi[idx2])
tpsim
tvar <- apply(tpsim, 1, var)/2
sp <- paste0('$set{', sp, '}$')
df <- data.frame(sp, pm, tm, tvar)

library(dplyr)
dist_table <- df %>% group_by(tm) %>% summarise(sum(pm))
tvt <- df %>% group_by(tvar) %>% summarise(sum(pm))