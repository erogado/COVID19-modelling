url = "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_camas_uci_2017.csv"
camas_uci = read.csv(url)
camas_uci = camas_uci[, -1]

colnames(camas_uci) = c("CCAA", "publicos", "provados", "total")

CCAA = c("Andalucia", "Aragon", "Asturias", "Navarra", "Canarias", "Cantabria", "Castilla y Leon",
         "Castilla La Mancha", "Cataluna", "Valencia", "Extremadura", "Galicia",
         "Baleares", "La Rioja-Ceuta-Melilla", "Madrid", "Pais Vasco", "Murcia")

camas_uci$CCAA = CCAA

camas_uci