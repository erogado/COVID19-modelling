functions {
  
  real[] SEIR_mod(
    real t, // tiempo
    real[] y, // estado del sistema (Susceptible, Expuesto, Infectado, Removido)
    real[] params, // parámetros (beta_1, beta_2, épsilon, gamma)
    real[] x_r,
    int[] x_i
  ) {
    
    real dydt[4];
    
    dydt[1] = -(params[1] * y[1] * y[2]) - (params[2] * y[1] * y[3]);
    dydt[2] = (params[1] * y[1] * y[2] + params[2] * y[1] * y[3]) - (params[3] * y[2]);
    dydt[3] = (params[3] * y[2]) - (params[4] * y[3]);
    dydt[4] = params[4] * y[3];
   
    return dydt;
    
  }
  
}

data {
  
  int <lower = 1> n_params; // introducir el número de parámetros del modelo
  int <lower = 1> n_obs; // número de de dias observados (tamaño de la muestra)
  int <lower = 1> n_difeq; // número de ODEs del sistema
  real t0; // tiempo inicial
  real ts[n_obs]; // tiempos muestreados
  int <lower = 1> n_pop; // población total
  int y[n_obs]; // datos -> número de infectados diarios
  int <lower = 1> n_fake; // número de predicciones
  real fake_ts[n_fake]; // tiempos en los que predecir
  
}

transformed data {
  
  real x_r[0];
  int x_i[0];
  
}

parameters {
  
  real <lower = 0> params[n_params]; // parámetros del modelo (beta_1, beta_2, épsilon, gamma)
  real <lower = 0, upper = 1> S0; // proporción de susceptibles iniciales
  
}

transformed parameters {
  
  real y_hat[n_obs, n_difeq]; // resultado de las ODEs
  real y_init[n_difeq]; // condiciones iniciales SEIR
  
  y_init[1] = S0;
  y_init[2] = 1 - S0;
  y_init[3] = 0;
  y_init[4] = 0;
  
  y_hat = integrate_ode_rk45(SEIR_mod, y_init, t0, ts, params, x_r, x_i);
  
}

model {
  
  real lambda[n_obs]; // parámetro para el modelo poisson
  
  // Priors para los parámetros
  params[1] ~ uniform(0, 2);
  params[2] ~ uniform(0, 2);
  params[3] ~ normal(0.2, 0.001);
  params[4] ~ normal(0.14, 0.001);
  S0 ~ beta(0.5, 0.5);
  
  // Likelihood Poisson del modelo
  for(i in 1:n_obs) {
    lambda[i] = y_hat[i, 2] * n_pop + y_hat[i, 3] * n_pop;
  }
  
  y ~ poisson(lambda);
  
}

generated quantities {
  
  real R_0;
  real fake_I[n_fake, n_difeq];
  
  R_0 = (params[1] + params[2]) / params[4];
  fake_I = integrate_ode_rk45(SEIR_mod, y_init, t0, fake_ts, params, x_r, x_i);
  
}