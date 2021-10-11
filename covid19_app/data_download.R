# Carga de los datos nacionales --------------------------------------------------------------------------
url = "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19.csv"
datos_nac = read.csv2(url, sep = ",")
colnames(datos_nac)[1] = "Fecha"

N = 47100396
datos1 = data.frame(
  Fecha = datos_nac$Fecha,
  Susceptibles = N - datos_nac$casos_total,
  Infectados = datos_nac$casos_total,
  Altas = datos_nac$altas,
  Fallecidos = datos_nac$fallecimientos,
  Removidos = datos_nac$altas + datos_nac$fallecimientos
)

rm(url)