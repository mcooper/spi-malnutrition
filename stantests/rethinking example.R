library(devtools)
devtools::install_github("rmcelreath/rethinking")

library(rethinking)

N <- 100
N_miss <- 10
x <- rnorm( N )
y <- rnorm( N , 2*x , 1 )
x[ sample(1:N,size=N_miss) ] <- NA

f5 <- alist(
  y ~ dnorm( mu , sigma ),
  mu <- a + b*x,
  x ~ dnorm( mu_x, sigma_x ),
  a ~ dnorm( 0 , 100 ),
  b ~ dnorm( 0  , 10 ),
  mu_x ~ dnorm( 0 , 100 ),
  sigma_x ~ dcauchy(0,2),
  sigma ~ dcauchy(0,2)
)
m5 <- map2stan( f5 , data=list(y=y,x=x) )

stancode(m5)


stanmodelcode <- "
data{
  int<lower=1> N;
  int<lower=1> N_x_missing;
  real y[N];
  real x[N];
  int x_missingness[N_x_missing];
}
parameters{
  vector[N_x_missing] x_impute;
  real a;
  real b;
  real mu_x;
  real<lower=0> sigma_x;
  real<lower=0> sigma;
}
transformed parameters{
  real x_merge[N];
  x_merge = x;
  for ( u in 1:N_x_missing ) x_merge[x_missingness[u]] = x_impute[u];
}
model{
  vector[N] mu;
  sigma ~ cauchy( 0 , 2 );
  sigma_x ~ cauchy( 0 , 2 );
  mu_x ~ normal( 0 , 100 );
  b ~ normal( 0 , 10 );
  a ~ normal( 0 , 100 );
  x_merge ~ normal( mu_x , sigma_x );
  for ( i in 1:N ) {
    mu[i] = a + b * x_merge[i];
  }
  y ~ normal( mu , sigma );
}
generated quantities{
  vector[N] mu;
  real dev;
  dev = 0;
  for ( i in 1:N ) {
    mu[i] = a + b * x_merge[i];
  }
  dev = dev + (-2)*normal_lpdf( y | mu , sigma );
}
"

dat <- list(y=y,x=x)

fit = stan(model_code=stanmodelcode, data=dat, iter=12000,
           warmup=2000, thin=10, chains=3)
