data {
  int<lower=0> N;
  int<lower=0> N_obs;
  int<lower=0> J;
  vector[N_obs] y_obs;
  int visit[N];
  int<lower=1, upper=J> id[N];
}

transformed data{
  int N_miss = N - N_obs;
}

parameters {
  vector[N_miss] y_miss;
  
  vector[J] alpha;
  vector[J] beta;
  
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  
  real mu_a;
  real mu_b;
}

transformed parameters{
  vector[N] mu;
  vector[N_obs] mu_obs;
  vector[N_miss] mu_miss;
  
  for(i in 1:N)
    mu[i] = alpha[id[i]] + beta[id[i]] * visit[i];
  
  mu_obs = head(mu, N_obs);
  mu_miss = tail(mu, N_miss); 
}

model {
  mu_a ~ normal(0, 100);
  mu_b ~ normal(0, 100);
  
  sigma_y ~ uniform(0, 100);
  sigma_a ~ uniform(0, 100);
  sigma_b ~ uniform(0, 100);
  
  for(j in 1:J){
    alpha[j] ~ normal(mu_a, sigma_a);
    beta[j] ~ normal(mu_b, sigma_b);
  }
  
  y_obs ~ normal(mu_obs, sigma_y);
  y_miss ~ normal(mu_miss, sigma_y);
}
