library(rstan)

##################################
#Example of Bayesian principles from http://m-clark.github.io/docs/IntroBayes.html
###################################


#' Lets look at the probability a random car is texting
#' We have ten observations of cars.  6 were texting, 4 weren't

drive = c('texting','texting','texting','not','not',
          'texting','texting','not','not','texting')

driveNum = ifelse(drive=='texting', 1, 0)
N = length(drive)                      # sample size
nTexting = sum(drive=='texting')       # number of drivers texting
nNot = sum(drive=='not')               # number of those not

#' Play with binomial distributions, because samples of 10 cars 
#' where each is texting or not will be binomially distributed

x1 = rbinom(1000, size=10, p=.5)   #How many texters would you expect in 10 cars if there is a 50% chance of texting?
x2 = rbinom(1000, size=10, p=.85)  #How many texters would you expect in 10 cars if there is a 85% chance of texting?

mean(x1); hist(x1)
mean(x2); hist(x2)

#' Develop a prior distribution

theta = seq(from=1/(N+1), to=N/(N+1), length=10) #some probabilities spaced from 0 to 1

# we could pick a uniform distribution
# pTheta = dunif(theta)

# we could also pick a beta distribution with mean = .5
# pTheta = dbeta(theta, 10, 10)

# triangular as in Kruschke
pTheta = pmin(theta, 1-theta)

pTheta = pTheta/sum(pTheta) # Normalize so sum to 1

#' Here we get the likelihood of the observed data given each of the thetas we are testing
#' The frequentist approach normally ends here
#' This math comes from the hte likelihood function of the binomail distribution - p(data|hypothesis)
pDataGivenTheta = choose(N, nTexting) * theta^nTexting * (1-theta)^nNot

#' Now we get the marginal probability of the data that we have observed p(data)
pData = sum(pDataGivenTheta*pTheta)

#' Now we apply bayes theorem to get p(hypothesis|data)
pThetaGivenData = pDataGivenTheta*pTheta  / pData  # Bayes theorem

plot(pThetaGivenData)


########################################
#Simple BHM also from http://m-clark.github.io/docs/IntroBayes.html
#########################################


# set seed for replicability
set.seed(8675309)

# create a N x k matrix of covariates
N = 250
K = 3

covariates = replicate(K, rnorm(n=N))
colnames(covariates) = c('X1', 'X2', 'X3')

# create the model matrix with intercept
X = cbind(Intercept=1, covariates)

# create a normally distributed variable that is a function of the covariates
coefs = c(5,.2,-1.5,.9)
mu = X %*% coefs
sigma = 2
y <- rnorm(N, mu, sigma)

# same as
# y = 5 + .2*X1 - 1.5*X2 + .9*X3 + rnorm(N, mean=0, sd=2)

# Run lm for later comparison; but go ahead and examine now if desired
modlm = lm(y~., data=data.frame(X[,-1]))
# summary(modlm)


dat = list(N=N, K=ncol(X), y=y, X=X)


# Create the stan model object using Stan's syntax
stanmodelcode = "
data {                    // Data block
  int<lower=1> N;           // Sample size
  int<lower=1> K;           // Dimension of model matrix
  matrix[N, K] X;           // Model Matrix
  vector[N] y;              // Target variable
}

/* 
transformed data {        // Transformed data block. Not used presently.
} 
*/

parameters {              // Parameters block
  vector[K] beta;           // Coefficient vector
  real<lower=0> sigma;      // Error scale
}

model {                   // Model block
  vector[N] mu;
  mu = X * beta;            // Creation of linear predictor
  
  // priors
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 5);     // With sigma bounded at 0, this is half-cauchy
  
  // likelihood
  y ~ normal(mu, sigma);
}

/*
generated quantities {    // Generated quantities block. Not used presently.
}
*/
"

fit = stan(model_code=stanmodelcode, data=dat, iter=12000,
           warmup=2000, thin=10, chains=3)


#########################
#Figure out missing data!
#########################


N_tot <- 1000
N_mis <- 150
N_obs <- N_tot - N_mis

x <- rnorm(N_tot, 100, 10)

intercept <- rnorm(N_tot, -10000, 10000)

y <- intercept + 1000*x

x[sample(seq(1, N_tot), N_mis)] <-NA

x_ii_mis <- which(is.na(x))
x_ii_obs <- which(!is.na(x))

x_obs <- x[x_ii_obs]

dat <- list(N_tot, N_mis, N_obs, x_obs, y, x_ii_mis, x_ii_obs)

stanmodelcode <- "
data {
  int<lower=0> N_tot;
  int<lower=0> N_mis;
  int<lower=0> N_obs;
  int<lower=1, upper = N_tot> x_ii_obs[N_obs];
  int<lower=1, upper = N_tot> x_ii_mis[N_mis];
  
  real x_obs[N_obs];
  real y[N_tot];
}
parameters {
  real x_mis[N_mis];
  real<lower=0> sigma;
  
  real beta0;
  real beta1;
}
transformed parameters {
  real x[N_tot];
  x[x_ii_obs] = x_obs;
  x[x_ii_mis] = x_mis;
}

model {
  sigma ~ gamma(1, 1);
  y ~ normal(beta0 + beta1 * to_vector(x), sigma);
}
"

fit = stan(model_code=stanmodelcode, data=dat, iter=12000,
           warmup=2000, thin=10, chains=3)







################################
#Try again with DHS data
################################

dat <- read.csv(file = 'G://My Drive/DHS Processed/BHM_testdata.csv')

library(dplyr)

dat <- dat %>% select(haz_dhs, age, hhsize, wealth_index, mother_years_ed, birth_order, 
                      watersource_dist, mother_age, breast_duration, father_age, father_years_ed) %>%
  mutate(wealth_index=ifelse(wealth_index=="Poorest", 1, 
                             ifelse(wealth_index=="Poorer", 2, 
                                    ifelse(wealth_index=="Middle", 3,
                                           ifelse(wealth_index=="Richer", 4, 5)))))

mod <- lm(haz_dhs~age+hhsize+wealth_index+mother_years_ed+birth_order+watersource_dist+mother_age+breast_duration+father_age+father_years_ed, data=dat)
summary(mod)

y <- dat$haz_dhs
X <- as.matrix(dat %>% select(-haz_dhs))

dat = list(N=nrow(X), K=ncol(X), y=y, X=X)
                                                                                                  
stanmodelcode = "
data {                      // Data block
int<lower=1> N;           // Sample size
int<lower=1> K;           // Dimension of model matrix
matrix[N, K] X;           // Model Matrix
vector[N] y;              // Target variable
}

/* 
transformed data {          // Transformed data block. Not used presently.
} 
*/

parameters {                // Parameters block
vector[K] beta;           // Coefficient vector
real<lower=0> sigma;      // Error scale
}

model {                     // Model block
vector[N] mu;
mu <- X * beta;           // Creation of linear predictor

// priors
beta ~ normal(0, 10);
sigma ~ cauchy(0, 5);     // With sigma bounded at 0, this is half-cauchy

// likelihood
y ~ normal(mu, sigma);
}

/*
generated quantities {      // Generated quantities block. Not used presently.
}
*/
"

fit = stan(model_code=stanmodelcode, data=dat, iter=1000,
           warmup=200, thin=10, chains=2)





