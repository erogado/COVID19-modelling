to_date = function(vec) {
  
  new_dates = c()
  for(i in 1:length(vec)) {
    new_date = substr(vec[i], start = 2, stop = nchar(vec[i]))
    substr(new_date, 5, 5) = "-"
    substr(new_date, 8, 8) = "-"
    new_date = as.Date(new_date, format = "%Y-%m-%d")
    new_dates[i] = as.character(new_date)
    
  }
  
  new_dates = as.Date(new_dates)
  return(new_dates)
  
}

data_list = function(d_casos, d_fallecimientos, d_altas) {
  
  # Primero, obtener la secuencia total de fechas para homogenizar los datos
  f_casos = d_casos[, c(ncol(d_casos))]
  f_altas = d_altas[, c(ncol(d_altas))]
  f_fallecidos = d_fallecimientos[, c(ncol(d_fallecimientos))]
  
  f_max = max(max(f_casos), max(f_altas), max(f_fallecidos))
  f_min = min(min(f_casos), min(f_altas), min(f_fallecidos))
  
  fechas = seq(f_min, f_max, 1)
  
  # Listado de comunidades
  comunidades = c("Andalucia", "Aragon", "Asturias", "Baleares", "Canarias", "Cantabria",
                  "Castilla La Mancha", "Castilla y Leon", "Cataluna", "Ceuta", "C.Valenciana",
                  "Extremadura", "Galicia", "Madrid", "Melilla", "Murcia", "Navarra", "Pais Vasco",
                  "La Rioja", "Total")
  
  # Creación de la lista de datasets 
  dnew_list = list()
  for(k in 1:length(comunidades)) {
    d_c = d_casos[, c(k, ncol(d_casos))]
    d_a = d_altas[, c(k, ncol(d_altas))]
    d_f = d_fallecimientos[, c(k, ncol(d_fallecimientos))]
    
    dataset = cbind.data.frame(fechas)
    vec_casos = c()
    vec_altas = c()
    vec_fallecimientos = c()
    for(i in 1:length(fechas)) {
      for(ci in 1:nrow(d_c)) {
        if(fechas[i] == d_c[ci, 2]) {
          vec_casos[i] <- d_c[ci, 1]
          
        }
        
      }
      
      for(ai in 1:nrow(d_a)) {
        if(fechas[i] == d_a[ai, 2]) {
          vec_altas[i] <- d_a[ai, 1]
          
        }
        
      }
      
      for(fi in 1:nrow(d_f)) {
        if(fechas[i] == d_f[fi, 2]) {
          vec_fallecimientos[i] <- d_f[fi, 1]
          
        }
        
      }
      
    }
    
    dataset = cbind.data.frame("Fecha" = fechas,
                               "Casos" = vec_casos, 
                               "Fallecidos" = vec_fallecimientos,
                               "Altas" = vec_altas)
    
    dnew_list[[k]] <- dataset
    
  }
  
  names(dnew_list) = comunidades
  return(dnew_list)
  
}