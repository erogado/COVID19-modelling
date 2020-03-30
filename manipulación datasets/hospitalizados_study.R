# Lectura de los datos
url = "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_hospitalizados.csv"
hospitalizados = read.csv(url)

# Almaceno en un vector las CCAA y el total nacional
comunidades = c("Andalucia", "Aragon", "Asturias", "Baleares", "Canarias", "Cantabria",
                "Castilla La Mancha", "Castilla y Leon", "Cataluna", "Ceuta", "C.Valenciana",
                "Extremadura", "Galicia", "Madrid", "Melilla", "Murcia", "Navarra", "Pais Vasco",
                "La Rioja", "Total")

# Elimino el id de las CCAA y las CCAA
hospitalizados = hospitalizados[, -c(1:2)]

# Transpongo los datos y los mantenfo en df
hospitalizados = data.frame(t(hospitalizados))

# Asigno el nombre de las columnas como las CCAA
colnames(hospitalizados) = c(comunidades)

# Manipulación de fechas
fechas = row.names(hospitalizados)

source("my_functions.R")
fechas = to_date(fechas)

hospitalizados$Fechas = fechas
row.names(hospitalizados) = seq(1, nrow(hospitalizados), 1)

# Lo que importa de todo el proceso anterior
rm(comunidades, fechas, url, to_date)
hospitalizados
