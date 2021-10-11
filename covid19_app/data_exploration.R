source("data_download.R")

cnames = c("Date", "Infected", "Recovered", "Deaths", "Hospitalized", "Label")

# Getting the data wa are interested in
data_model = datos_nac[6:88, ]
data_pred = datos_nac[89:91, ]

data_model = data_model[, c(1, 2, 5, 6, 8)]
data_pred = data_pred[, c(1, 2, 5, 6, 8)]

data_model$Label = rep("To the model", nrow(data_model))
data_pred$Label = rep("To predict", nrow(data_pred))

data_plot = rbind(data_model, data_pred)
colnames(data_plot) = cnames

rm(data_model, data_pred, datos1)



