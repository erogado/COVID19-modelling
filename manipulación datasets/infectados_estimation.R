source("rango_edad_study.R")

# Ratio de fallecidos estimado por franja de edad ->
# de momento se han cogido los de Corea a 28/03/2020
f_ratio = c(0, 0, 0, 0.1, 0.08, 0.56, 1.74, 6.77, 17.51, 1.59)/100

# Porcentaje que supone la población con un ratio de mortalidad de 0
pob_0_29 = (14.05 + 15.19)/100


# La función está hecha para los siguinetes rangos de edad: 
# (0-9, 10-19, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80+)
infected_est.V1 = function(datos, sexo_target, f_ratios, p_pob) {
  
  library(dplyr)
  dat = datos %>% 
    filter(sexo == sexo_target) %>% 
    select(-sexo)
  
  mod_data = NULL
  fecha_un = unique(dat$Fecha)
  for(i in 1:length(fecha_un)) {
    dat_aux = subset(dat, dat$Fecha == fecha_un[i])
    dat_aux$rango_edad[dat_aux$rango_edad == "80-89"] <- "80 y +"
    dat_aux$rango_edad[dat_aux$rango_edad == "90 y +"] <- "80 y +"
    
    dat_aux = dat_aux %>% 
      group_by(rango_edad) %>% 
      summarise(casos_confirmados = sum(casos_confirmados), 
                hospitalizados = sum(hospitalizados), 
                ingresados_uci = sum(ingresados_uci),
                fallecidos = sum(fallecidos))
    
    dat_aux = data.frame(dat_aux)
    
    mod_data = rbind(mod_data, dat_aux)
    
  }
  
  dat = mod_data
  
  f_ratio = rep(f_ratio, length.out = nrow(dat))
  infectados = dat$fallecidos / f_ratio
  
  dat$est_infectados = infectados
  dat$est_infectados[dat$est_infectados == Inf] <- 0
  dat$est_infectados[is.na(dat$est_infectados)] <- 0
  
  seq_rows = seq(1, 9, 1)
  new_est_inf = NULL
  n_max = max(seq_rows)
  while(n_max <= nrow(dat)) {
    
    aux_dat = dat[seq_rows[1]:seq_rows[length(seq_rows)], ]
    est_inf = sum(aux_dat[, 6]) / (1 - pob_0_29)
    
    n_ceros = sum(ifelse(aux_dat$est_infectados == 0, 1, 0))
    
    aux_dat$est_infectados = ifelse(aux_dat$est_infectados == 0, ((est_inf*pob_0_29) / n_ceros), aux_dat$est_infectados)
    
    new_est_inf = c(new_est_inf, aux_dat$est_infectados, dat$est_infectados[(max(seq_rows) + 1)])
    
    seq_rows = seq_rows + 10
    n_max = max(seq_rows)
    
    if( (n_max > nrow(dat)) & (nrow(dat) %in% seq_rows) ) {
      c = 0
      for(i in 1:length(seq_rows)) {
        if(seq_rows[i] == nrow(dat)) {
          c = i
          break
        }
        
      }
      
      seq_rows = seq_rows[1:c]
      n_max = nrow(dat)
      
    } else {
      seq_rows = seq_rows
      n_max = n_max
      
    }   
    
    
  }
  
  dat$est_infectados = round(new_est_inf, 0)
  
  return(dat)
  
}
infected_est.V1(datos = rango_edad, sexo_target = "ambos", f_ratios = f_ratio, p_pob = pob_0_29)

