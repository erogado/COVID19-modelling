source("casos_study.R")
source("fallecidos_study.R")
source("altas_study.R")
source("hospitalizados_study.R")
source("uci_study.R")
source("my_functions.R")

data = data_list(casos, fallecidos, altas, hospitalizados, uci)
rm(data_list, to_date, altas, casos, fallecidos, uci, hospitalizados)
data