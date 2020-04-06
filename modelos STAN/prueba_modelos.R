
# Preparación de los datos --------------------------------------------------------------------------
source("navigation_list.R")

dat = data[["Total"]]
dat$Fallecidos[is.na(dat$Fallecidos)] <- 0
dat$Altas[is.na(dat$Altas)] <- 0

#dat = dat[, c(1, 2, 4)]
dat = data.frame(
  Fecha = dat$Fecha,
  Susceptibles = (46660000 - dat$Casos - dat$Altas - dat$Fallecidos),
  Infectados = dat$Casos,
  Removidos = dat$Altas + dat$Fallecidos
)


plot(dat$Infectados/46660000, type = "o", cex = 0.6, pch = 19)
lines(dat$Removidos/46660000, type = "o", cex = 0.6, pch = 19, col = "red")
lines(diff(dat$Infectados)/46660000, type = "o", cex = 0.6, pch = 19, col = "blue")

plot(dat$Susceptibles/46660000, type = "o", cex = 0.6, pch = 19)

diff(dat$Infectados)

# MODELO SIR CON STAN (1) -----------------------------------------------------------------------------
library(StanHeaders)
library(rstan)
rstan_options(auto_write = T)
# Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
options(mc.cores = parallel::detectCores())

infected = dat$Infectados
times = 1:nrow(dat)
N = nrow(dat)
pop = 46660000

covid_data = list(
  n_obs = N,
  n_params = 2,
  n_difeq = 3,
  n_pop = pop,
  y = infected,
  t0 = 0,
  ts = times,
  n_fake = N + 10,
  fake_ts = c(1:(N+10))
)

parameters_control = c ("y_hat", "y_init", "params" , "R_0", "fake_I")

n_chains = 4
n_warmups = 250000
n_iter = 500000
n_thin = 5

init_values = function() {
  list(
    params = c(runif(1, 0, 5), runif(1, 0, 0.1)),
    S0 = runif(1, (pop-3)/pop, (pop-1)/pop)
  )
}

t = proc.time()
fit = stan( 
  file = "SIR_poisson.stan",
  data = covid_data,
  pars = parameters_control,
  init = init_values,
  chains = n_chains,
  warmup = n_warmups,
  iter = n_iter,
  thin = n_thin,
  control = list(max_treedepth = 15, adapt_delta = 0.8),
  cores = 4
)
proc.time() - t

post = extract(fit)

predictions = post$fake_I
predictions_means = colMeans(predictions)

prop_inf = dat$Infectados / 46660000

# PLOT INFECTADOS
v_abs_pred = round(predictions_means[, 2] * 46660000, 0)
v_abs_real = prop_inf * 46660000
plot(v_abs_pred, type = "o", col = "black", cex = 1, pch = 19, ylim = c(0, 200000))
lines(v_abs_real, col = "red", type = "o", pch = 19, cex = 0.5)

# PLOT SUSCEPTIBLES
v_abs_pred = round(predictions_means[, 1] * 46660000, 0)[1:29]
v_abs_real = dat[, 2]
plot(v_abs_pred, type = "o", col = "black", cex = 0.75, pch = 19)
lines(v_abs_real, col = "red", type = "o", pch = 19, cex = 0.5)

# PLOT REMOVIDOS
v_abs_pred = round(predictions_means[, 4] * 46660000, 0)[1:29]
v_abs_real = dat[, 4]
plot(v_abs_pred, type = "o", col = "black", cex = 0.75, pch = 19)
lines(v_abs_real, col = "red", type = "o", pch = 19, cex = 0.5)


plot(predictions_means[, 3], type = "o", col = "black", cex = 0.75, pch = 19)
lines(dat$Removidos/46660000, col = "red", type = "o", pch = 19, cex = 0.5)

# DISTRIBUCIÓN DE LOS PARÁMETROS ESTIMADOS (1) ----------------------------------------------------------
hist(post$R_0)
hist(post$params[, 1])
hist(post$params[, 2])

mean(post$R_0)
mean(post$params[, 1])
mean(post$params[, 2])


# MODELO SEIR CON STAN (2) ------------------------------------------------------------------------------
library(StanHeaders)
library(rstan)
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())

infected = dat$Infectados
times = 1:nrow(dat)
N = nrow(dat)
pop = 46660000

covid_data = list(
  n_obs = N,
  n_params = 3,
  n_difeq = 4,
  n_pop = pop,
  y = infected,
  t0 = 0,
  ts = times,
  n_fake = N + 10,
  fake_ts = c(1:(N+10))
)

parameters_control = c ("y_hat", "y_init", "params" , "R_0", "fake_I")

n_chains = 4
n_warmups = 250000
n_iter = 500000
n_thin = 10

init_values = function() {
  list(
    params = c(runif(1, 0, 3), runif(1, 0, 0.2), runif(1, 0, 0.1)),
    S0 = runif(1, (pop-3)/pop, (pop-1)/pop)
  )
}

t = proc.time()
fit = stan( 
  file = "SEIR_poisson.stan",
  data = covid_data,
  pars = parameters_control,
  init = init_values,
  chains = n_chains,
  warmup = n_warmups,
  iter = n_iter,
  thin = n_thin,
  control = list(max_treedepth = 15, adapt_delta = 0.8),
  cores = 4
)
proc.time() - t

post = extract(fit)

predictions = post$fake_I
predictions_means = colMeans(predictions)

prop_inf = dat$Infectados / 46660000

# PLOT INFECTADOS
v_abs_pred = round(predictions_means[, 3] * 46660000, 0)
v_abs_real = prop_inf * 46660000
plot(v_abs_pred, type = "o", col = "black", cex = 1, pch = 19)
lines(v_abs_real, col = "red", type = "o", pch = 19, cex = 0.5)

# PLOT SUSCEPTIBLES
v_abs_pred = round(predictions_means[, 1] * 46660000, 0)
v_abs_real = dat[, 2]
plot(v_abs_pred, type = "o", col = "black", cex = 0.75, pch = 19)
lines(v_abs_real, col = "red", type = "o", pch = 19, cex = 0.5)

# PLOT REMOVIDOs
plot(predictions_means[, 4], type = "o", col = "black", cex = 0.75, pch = 19)
lines(dat$Removidos/46660000, col = "red", type = "o", pch = 19, cex = 0.5)

# DISTRIBUCIÓN DE LOS PARÁMETROS ESTIMADOS (2) ----------------------------------------------------------
hist(post$R_0)
hist(post$params[, 1])
hist(post$params[, 2])
hist(post$params[, 3])

mean(post$R_0)
mean(post$params[, 1])
mean(post$params[, 2])
mean(post$params[, 3])






# MODELO SEIR MODIFICADO (3) -------------------------------------------------------------------------
library(StanHeaders)
library(rstan)
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())

infected = dat$Infectados
times = 1:nrow(dat)
N = nrow(dat)
pop = 46660000

covid_data = list(
  n_params = 4,
  n_obs = N,
  n_difeq = 4,
  t0 = 0,
  ts = times,
  n_pop = pop,
  y = infected,
  n_fake = N + 10,
  fake_ts = c(1:(N+10))
)

parameters_control = c ("y_init", "params" , "R_0", "fake_I")
# "y_hat", 

n_chains = 4
n_warmups = 250000
n_iter = 500000
n_thin = 10

init_values = function() {
  list(
    params = c(runif(1, 0, 2), runif(1, 0, 2), runif(1, 0, 0.2), runif(1, 0, 0.1)),
    S0 = runif(1, (pop-3)/pop, (pop-1)/pop)
  )
}

t = proc.time()
fit = stan( 
  file = "SEIR_mod_poisson.stan",
  data = covid_data,
  pars = parameters_control,
  init = init_values,
  chains = n_chains,
  warmup = n_warmups,
  iter = n_iter,
  thin = n_thin,
  control = list(max_treedepth = 15, adapt_delta = 0.8),
  cores = 4
)
proc.time() - t

post = extract(fit)

predictions = post$fake_I
predictions_means = colMeans(predictions)

prop_inf = dat$Infectados / 46660000

# PLOT INFECTADOS   + predictions_means[, 2]
v_abs_pred = round((predictions_means[, 3]) * 46660000, 0)
v_abs_real = prop_inf * 46660000
plot(v_abs_pred, type = "o", col = "black", cex = 1, pch = 19, ylim = c(0, 600000))
lines(v_abs_real, col = "red", type = "o", pch = 19, cex = 0.5)

# PLOT SUSCEPTIBLES
v_abs_pred = round(predictions_means[, 1] * 46660000, 0)
v_abs_real = dat[, 2]
plot(v_abs_pred, type = "o", col = "black", cex = 0.75, pch = 19)
lines(v_abs_real, col = "red", type = "o", pch = 19, cex = 0.5)


# PLOT REMOVIDOS
plot(predictions_means[, 4], type = "o", col = "black", cex = 0.75, pch = 19)
lines(dat$Removidos/46660000, col = "red", type = "o", pch = 19, cex = 0.5)

# DISTRIBUCIÓN DE LOS PARÁMETROS ESTIMADOS (3) ----------------------------------------------------------
hist(post$R_0)
hist(post$params[, 1])
hist(post$params[, 2])
hist(post$params[, 3])
hist(post$params[, 4])

mean(post$R_0)
mean(post$params[, 1])
mean(post$params[, 2])
mean(post$params[, 3])
mean(post$params[, 4])

