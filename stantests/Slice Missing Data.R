library(rstan)

stancode <- "
data {
  int<lower = 0> N_obs;
  int<lower = 0> N_mis;
  int<lower = 1, upper = N_obs + N_mis> ii_obs[N_obs];
  int<lower = 1, upper = N_obs + N_mis> ii_mis[N_mis];
  real y_obs[N_obs];
}
transformed data {
  int<lower = 0> N = N_obs + N_mis;
}
parameters {
  real y_mis[N_mis];
  real<lower=0> sigma;
}
transformed parameters {
  real y[N];
  y[ii_obs] = y_obs;
  y[ii_mis] = y_mis;
}
model {
  sigma ~ gamma(1, 1);
  y[1] ~ normal(0, 100);
  y[2:N] ~ normal(y[1:(N - 1)], sigma);
}


"

y <- rnorm(115, 0, 15)

y[sample(seq(1, 115), 15)] <- NA

ii_obs <- which(!is.na(y))
ii_mis <- which(is.na(y))

y_obs <- y[ii_obs]

dat = list(N_obs=100, N_mis=15, ii_obs, ii_mis, y_obs)

fit = stan(model_code = stancode, data = dat)
