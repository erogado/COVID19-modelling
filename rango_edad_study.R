url = "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19_rango_edad.csv"
rango_edad = read.csv(url)

colnames(rango_edad) = c("Fecha", "sexo", "rango_edad", "casos_confirmados", 
                         "hospitalizados", "ingresados_uci", "fallecidos")

rango_edad
