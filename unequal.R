Mi <- c(50,100,150,200, 500)
ti <- c(6, 10, 18, 26, 40)

psi <- Mi/sum(Mi)
tpsi <- ti/psi

# pps
Etht <- sum(ti/psi*psi)
Vtht <- sum((tpsi-Etht)^2*psi)
Esrs <- sum(ti/.2*.2)
Vsrs <- sum((ti/.2-Etht)^2*.2)
# Eratio <- 

#srswr
sp <- 1:5

idx <- t(outer(sp, sp, 'paste0'))
sp <- t(outer(sp, sp, paste, sep = ','))
sp <- sp[lower.tri(sp)]

psiwr <- t(outer(psi, psi)) * 2
diag(psiwr) <- diag(psiwr)/2
psiwr <- psiwr[lower.tri(psiwr, diag = TRUE)]

idx <- as.numeric(idx[lower.tri(idx, diag = TRUE)])
idx1 <- idx %/% 10
idx2 <- idx %% 10
tpsi_sp <- tibble(pr = psiwr, s1 = tpsi[idx1], s2 = tpsi[idx2])

library(dplyr)
tpsi_sp <- tpsi_sp %>% mutate(mu = (s1+s2)/2) %>% mutate(var = ((s1-mu)^2+(s2-mu)^2)/2)
Esrswr <- sum(tpsi_sp$pr * tpsi_sp$mu)
Vsrswr <- sum(tpsi_sp$pr * tpsi_sp$var)
tpsi_sp %>% group_by(mu) %>% summarise(prt = sum(pr))
tvar <- tpsi_sp %>% group_by(var) %>% summarise(prvar = sum(pr))
sum(tvar$var*tvar$prvar)

#srswor
psiwor <- outer(psi, psi/(1-psi))
diag(psiwor) <- 0

pij <- psiwor[lower.tri(psiwor)] + t(psiwor)[lower.tri(t(psiwor))]
pi <- rep(NA, 5)
for(i in 1:5) pi[i] <- sum(psiwor[i,], psiwor[,i])

#Horvitz-Thompson estimator
idx <- combn(5,2)
titj <- ti[idx[1,]]*ti[idx[2,]]
pipj <- pi[idx[1,]]*pi[idx[2,]]

Vtht <- sum(ti^2*(1-pi)/pi) + 2*sum(titj*(pij-pipj)/pipj)

tht <- ti[idx[1,]]/pi[idx[1,]] + ti[idx[2,]]/pi[idx[2,]]

varht <- ti[idx[1,]]^2*(1-pi[idx[1,]])/pi[idx[1,]]^2 + 
  ti[idx[2,]]^2*(1-pi[idx[2,]])/pi[idx[2,]]^2 + 
  2*titj*(pij-pipj)/(pij*pipj)
varsyg <- (pipj-pij)/pij*(ti[idx[1,]]/pi[idx[1,]]-ti[idx[2,]]/pi[idx[2,]])^2

Etht2 <- sum(tht*pij)
Vtht2 <- sum(tht^2*pij) - Etht2^2

Vht <- sum(varht*pij)
Vsyg <- sum(varsyg*pij)