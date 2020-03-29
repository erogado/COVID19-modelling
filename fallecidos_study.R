# Lectura de los datos
url = "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos.csv"
fallecidos = read.csv(url)

# Almaceno en un vector las CCAA y el total nacional
comunidades = c("Andalucia", "Aragon", "Asturias", "Baleares", "Canarias", "Cantabria",
                "Castilla La Mancha", "Castilla y Leon", "Cataluna", "Ceuta", "C.Valenciana",
                "Extremadura", "Galicia", "Madrid", "Melilla", "Murcia", "Navarra", "Pais Vasco",
                "La Rioja", "Total")

# Elimino el id de las CCAA y las CCAA
fallecidos = fallecidos[, -c(1:2)]

# Transpongo los datos y los mantenfo en df
fallecidos = data.frame(t(fallecidos))

# Asigno el nombre de las columnas como las CCAA
colnames(fallecidos) = c(comunidades)

# Manipulación de fechas
fechas = row.names(fallecidos)

source("my_functions.R")
fechas = to_date(fechas)
fallecidos$Fechas = fechas
row.names(fallecidos) = seq(1, nrow(fallecidos), 1)

# Lo que importa de todo el proceso anterior
rm(comunidades, fechas, url, to_date)
fallecidos