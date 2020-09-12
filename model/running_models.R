source("national_data.R")

library(rstan)
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())

# infected counts
y = datos_nac$casos_total

# preparing the data
y = y[6:88]
ts = 1:length(y)
T = length(y)
t0 = 0
n_fake = T + 150
fake_ts = 1:(T+150)

# data list to introduce in the stan function
covid_data = list(
  T = T,
  N = N,
  y = y,
  t0 = t0,
  ts = ts,
  n_fake = n_fake,
  fake_ts = fake_ts
)

# parameters we want to control
parameters_control = c("y_init", "y_hat", "theta", "fake_I", "R_0")

# 
n_chains = 3
n_warmups = 25000
n_iter = 50000
n_thin = 3

# setting the initial values without eta
set.seed(80)
init_values = function() {
  list(
    theta = c(runif(1, 0, 5), runif(1, 0, 5), # beta1, beta2
              runif(1, 0.05, 0.5), # epsilon
              runif(1, 0.1, 0.2), runif(1, 0.1, 0.3), # kappa, lambda
              runif(1, 0.05, 0.08), runif(1, 0.02, 0.04),  # gamma1, gamma2
              runif(1, 0.02, 0.04), runif(1, 0.1, 0.4) # mu, omega
    ), 
    S0 = runif(1, (N-y[1])/N, (N-3)/N)
  )
}

# setting the initial values with eta
set.seed(80)
init_values = function() {
  list(
    theta = c(runif(1, 0, 5), runif(1, 0, 5), # beta1, beta2
              runif(1, 0.05, 0.5), # epsilon
              runif(1, 0.1, 0.2), runif(1, 0.1, 0.3), # kappa, lambda
              runif(1, 0.05, 0.08), runif(1, 0.02, 0.04),  # gamma1, gamma2
              runif(1, 0.02, 0.04), runif(1, 0.1, 0.4), #
              runif(1, 0, 100)
    ), 
    S0 = runif(1, (N-y[1])/N, (N-3)/N)
  )
}

# running the model
# SEI1I2RD_model1.stan -> model without eta
# SEI1I2RD_model2.stan -> model with eta
t = proc.time()
fit = stan( 
  file = "SEI1I2RD_model2.stan",
  data = covid_data,
  pars = parameters_control,
  init = init_values,
  chains = n_chains,
  warmup = n_warmups,
  iter = n_iter,
  thin = n_thin,
  control = list(max_treedepth = 15, adapt_delta = 0.8),
  cores = 4,
  seed = 1212
)
proc.time() - t