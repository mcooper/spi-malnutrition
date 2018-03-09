stanmod  <- "
data {
  int<lower=0> N_obs;
  int<lower=0> N_mis;
  real y_obs[N_obs];
  real useless[N_obs];
}
parameters {
  real mu;
  real<lower=0> sigma;
  real y_mis[N_mis];
}
model {
  y_obs ~ normal(mu, sigma);
  y_mis ~ normal(mu, sigma);
}

"

dat <- list(N_obs=100, N_mis=3, y_obs=rnorm(100, 0, 10), useless=sample(c(0,1.5,NA), 100, replace=TRUE))

fit <- stan(model_code=stanmod, data=dat)
