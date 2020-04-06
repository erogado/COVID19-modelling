functions {
  
  real[] SIR(real t, // tiempo
             real[] y, // estado del sitema (susceptible, infectados, recuperados)
             real[] params, // parámetros (transmision rate, recovery rate)
             real[] x_r, int[] x_i) { // real & integer values - fixed data -
      
      real dydt[3];
      
      dydt[1] = - params[1] * y[1] * y[2];
      dydt[2] = params[1] * y [1] * y [2] - params[2] * y[2];
      dydt[3] = params[2] * y [2];

      
      return dydt;
      
    }
  
}

data {
  
  int<lower = 1> n_obs; // número de días observados
  int<lower = 1> n_params; // número de parámetros del modelo
  int<lower = 1> n_difeq; // número de ODEs en el sistema
  int<lower = 1> n_pop; // población
  int y[n_obs]; // datos -> número total de infectados cada día
  real t0; // tiempo inicial
  real ts[n_obs]; // tiempos muestreados
  
  int<lower = 1> n_fake;
  real fake_ts[n_fake];
  
}

transformed data {
  real x_r[0];
  int x_i[0];
}

parameters {
  real<lower = 0> params[n_params]; // parámetros del modelo
  real<lower = 0, upper = 1> S0; // fracción de susceptibles iniciales
}

transformed parameters{
  
  real y_hat[n_obs, n_difeq]; // resultado de las ODEs
  real y_init[n_difeq]; // condiciones iniciales SIR

  y_init[1] = S0;
  y_init[2] = 1 - S0;
  y_init[3] = 0;
  
  y_hat = integrate_ode_rk45(SIR, y_init, t0, ts, params, x_r, x_i);
  
}

model {
  real lambda[n_obs]; // Parámetro poisson
  
  // priors
  params[1] ~ lognormal(0, 0.5);
  params[2] ~ normal(0.1, 0.001); 
  S0 ~ beta(0.5, 0.5); // tiene que ser entre 0-1
  
  // likelihood
  for(i in 1:n_obs) {
    lambda[i] = y_hat[i, 2]*n_pop;
  }
  
  y ~ poisson(lambda);
  //y ~ binomial(n_pop, y_hat[, 2]);
  
}

generated quantities {
  
  real R_0;
  real fake_I[n_fake, n_difeq];
  
  R_0 = params[1] / params[2];
  fake_I = integrate_ode_rk45(SIR, y_init, t0, fake_ts, params, x_r, x_i);
  
}
