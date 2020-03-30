url = "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19_rango_edad.csv"
rango_edad = read.csv(url)

colnames(rango_edad) = c("Fecha", "rango_edad", "sexo", "casos_confirmados", 
                         "hospitalizados", "ingresados_uci", "fallecidos")

rm(url)
rango_edad
