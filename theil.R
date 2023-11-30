library(tibble)
library(dplyr)
library(ggplot2)
library(patchwork)

x <- c(15, 15, 15, 20, 20, 20, 20, 40, 50, 60)
u <- mean(x)

# Theil index is based on Generalized Entropy index, which is given as follows,
# https://en.wikipedia.org/wiki/Generalized_entropy_index
# 0 < a < 1, GE(a) = mean((x/u)^a - 1)/(a*(a-1))
# a = 1, GE(1) = mean(x/u*log(x/u))
# a = 0, GE(0) = -mean(log(x/u))

# https://en.wikipedia.org/wiki/Theil_index
# Theil T index, a = 1
mean((x/u)*log(x/u))

# Theil L index, a = 0
-mean(log(x/u))

# encapsulation
TT <- function(x){return(mean((x/mean(x))*log(x/mean(x))))}
TL <- function(x){return(-mean(log(x/mean(x))))}

# perfect equality
y <- rep(20, 10)
TT(y)
TL(y)

# difference between T-T and T-L, test by example
xhigh <- c(10, rep(20, 8), 90)
xlow <- c(10, rep(80, 8), 90)

phigh <- ggplot(data = tibble(id = seq(x), xhigh), aes(as.factor(id), xhigh)) + 
  geom_point(size = 3) + theme_bw() + xlab('id') + ylab('x') + ggtitle('Difference at High End')

plow <- ggplot(data = tibble(id = seq(x), xlow), aes(as.factor(id), xlow)) + 
  geom_point(size = 3) + theme_bw() + xlab('id') + ylab('x') + ggtitle('Difference at Low End')

phigh + plow

TT(xhigh)
TL(xhigh)

TT(xlow)
TL(xlow)

# TT sensitive to high outlier
# TL sensitive to low outlier

# decomposition of Theil index
# simulate area
df <- tibble(area = c(rep(1, 2), rep(2, 5), rep(3, 3)), x)

N <- nrow(df)
xm <- mean(x)

# decomposition of Theil T
# compact version
dfTE <- df %>% group_by(area) %>% summarise(Ni = n(), xim = mean(x), TTi = TT(x), TLi = TL(x)) %>% 
  mutate(N = N, xm = xm, ri = Ni/N, si = ri * xim/xm) %>% 
  mutate(TTw = si * TTi, TTb = si * log(xim/xm), TLw = ri * TLi, TLb =  - ri * log(xim/xm))

# Theil_T idx
sum(dfTE$TTw) + sum(dfTE$TTb)
# verified by original TT function
TT(df$x)

# I_w = within group contribution, I_b = between group contribution
paste0(format(round(c(sum(dfTE$TTw), sum(dfTE$TTb))/TT(df$x), 4) * 100, nsmall = 2),  '%')

# Theil_T idx
sum(dfTE$TLw) + sum(dfTE$TLb)
# verified by original TL function
TL(df$x)

# I_w = within group contribution, I_b = between group contribution
paste0(format(round(c(sum(dfTE$TLw), sum(dfTE$TLb))/TL(df$x), 4) * 100, nsmall = 2),  '%')


# detail version
# T_T = sum(si * Ti) + sum(si * log(xim/xm)), where si = Ni/N * xim/xm
dfT <- df %>% group_by(area) %>% summarise(Ni = n(), xim = mean(x), TTi = TT(x)) %>% 
  mutate(N = N, xm = xm, si = Ni/N * xim/xm) %>% 
  mutate(TTw = si * TTi, TTb = si * log(xim/xm))

sum(dfT$TTw) + sum(dfT$TTb)
TT(df$x)

dfT$TTw
sum(dfT$TTb)

# I_w = within group contribution, I_b = between group contribution
paste0(round(c(sum(dfT$TTw), sum(dfT$TTb))/TT(df$x), 4) * 100, '%')

# decomposition of Theil L
# T_T = sum(ri * Ti) + sum(-ri * log(xim/xm)), where ri = Ni/N
dfL <- df %>% group_by(area) %>% summarise(Ni = n(), xim = mean(x), TLi = TL(x)) %>% 
  mutate(N = N, xm = xm, ri = Ni/N) %>% 
  mutate(TLw = ri * TLi, TLb =  - ri * log(xim/xm))

sum(dfL$TLw) + sum(dfL$TLb)
TL(df$x)

dfL$TLw
sum(dfL$TLb)

paste0(round(c(sum(dfL$TLw), sum(dfL$TLb))/TL(df$x), 4) * 100, '%')

# alternative computation, see https://www.ce-jeme.org/journal/vol4/iss2/1/
dfA <- df %>% mutate(N = n(), xm = mean(x)) %>% 
  group_by(area) %>%
  mutate(Ni = n(), xim = mean(x), ri = Ni/N, TLb =  - ri * log(xim/xm), TLw = log(x/xim)/N)

sum(unique(dfA$TLb)) - sum(dfA$TLw)
TL(df$x)

dfL %>% group_by(area) %>% summarise(TLw = sum(TLw)) %>% pull(TLw)



