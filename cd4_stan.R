library(dplyr)
library(rstan)
options(mc.cores = 3)
library(ggplot2)
library(patchwork)
library(bayesplot)

cd4 <- read.delim("cd4.txt", header = TRUE)
n_obs <- nrow(cd4)

J <- max(cd4$ID)
x <- seq(from = 1, by = 3, length.out = 7)
df <- tibble(id = rep(1:J, each = 7), visit = rep(x, J))

cd4 <- cd4 %>% select(id = ID, visit = Visit, CD4Pct) %>% 
  right_join(df, by = c('id', 'visit')) %>% 
  mutate(y = sqrt(CD4Pct))

n <- nrow(cd4)                  

cd4_data <- list(N = n, N_obs = n_obs, y_obs = cd4$y[1:n_obs], visit = cd4$visit, id = cd4$id, J = J)

fit <- stan(file='cd4.stan', data = cd4_data, chains = 3,
           iter = 2500, warmup = 2000, refresh = 1000, save_warmup = FALSE)


# find indices of y_miss correspond to id = 18 for example
idx <- which(cd4$id == 18)
idx[idx > n_obs] - n_obs

param <- c('y_miss[38]', 'y_miss[39]')

y18 <- extract(fit, pars = param)
sigmas <- extract(fit, pars = c('sigma_y', 'sigma_a', 'sigma_b'), permuted = FALSE)

id18 <- tibble(t16 = post_param$`y_miss[38]`, t19 = post_param$`y_miss[39]`)

id18 <- id18 %>% mutate(cd4t16 = t16^2, cd4t19 = t19^2)

mcmc_intervals(
  id18,
  pars = c('cd4t16', 'cd4t19'),
  prob = 0.95, # 80% intervals
  prob_outer = 0.95, # 99%
  point_est = "mean"
)

mcmc_trace(fit, pars = param)
