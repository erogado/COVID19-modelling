load("extended_model.Rdata")

fit2 = fit
rm(fit)

# parameters' estimations ------------------------------------------------------------------------------------------------
post_model2 = extract(fit2)
post_theta_model2 = data.frame(post_model2$theta, post_model2$R_0)
colnames(post_theta_model2) = c("beta1", "beta2", "epsilon", "kappa", "lambda", "gamma1", "gamma2", "mu","omega", "eta", "R0")

HDP_inter_model2 = NULL
for(i in 1:ncol(post_theta_model2)) {
  data = post_theta_model2[, i]
  inter = HPDinterval(as.mcmc(data), prob = 0.95)
  m = mean(data)
  a = inter[1]
  b = inter[2]
  amb = c(a, m, b)
  HDP_inter_model2 = rbind(HDP_inter_model2, amb)
  
}
HDP_inter_model2 = data.frame(HDP_inter_model2)
colnames(HDP_inter_model2) = c("2.5%", "50%", "97.5%")
rownames(HDP_inter_model2) = c("beta1", "beta2", "epsilon",  "kappa", "lambda", "gamma1", "gamma2", "mu", "omega", "eta", "R0")

rm(a, amb, b, data, i, m, inter)


# plots for the results -------------------------------------------------------------------
fitted_lines_m2 = cbind(as.data.frame(summary(fit2, pars = "y_hat", probs = c(0.05, 0.5, 0.95))$summary))
fitted_lines_m2 = fitted_lines_m2[, -c(ncol(fitted_lines_m2)-1, ncol(fitted_lines_m2))]
colnames(fitted_lines_m2) <- make.names(colnames(fitted_lines_m2)) # to remove % in the col names

ind_I1_flm2 = seq(3, nrow(fitted_lines_m2), 6)
ind_I2_flm2 = seq(4, nrow(fitted_lines_m2), 6)

y1 = datos_nac$casos_total[6:88] - datos_nac$hospitalizados[6:88]
y2 = datos_nac$hospitalizados[6:88]
y = datos_nac$casos_total[6:88]

fitted_lines_I_m2 = cbind(fitted_lines_m2[ind_I1_flm2, ]*N + fitted_lines_m2[ind_I2_flm2, ]*N, 
                       Type = rep("I_hat", length(ind_I2_flm2)),
                       time = seq(1, length(datos_nac$Fecha[6:88]), 1), I = y)

# ------------------------------------------------------------------------------------------------
pred_lines_m2 <- cbind(as.data.frame(summary(fit2, pars = "fake_I", probs = c(0.05, 0.5, 0.95))$summary))
pred_lines_m2 = pred_lines_m2[, -c(ncol(pred_lines_m2)-1, ncol(pred_lines_m2))]
colnames(pred_lines_m2) <- make.names(colnames(pred_lines_m2)) # to remove % in the col names

ind_I1_plm2 = seq(3, nrow(pred_lines_m2), 6)
ind_I2_plm2 = seq(4, nrow(pred_lines_m2), 6)

pred_lines_I_m2 = cbind(pred_lines_m2[ind_I1_plm2, ]*N + pred_lines_m2[ind_I2_plm2, ]*N, 
                     Type = rep("I_hat", length(ind_I2_plm2)),
                     time = seq(1, length(ind_I1_plm2), 1))

pred_lines_I_m2$y = c(datos_nac$casos_total[6:91], rep(NA, nrow(pred_lines_I_m2) - 86))

plot_pred_m2 = pred_lines_I_m2[1:86, ]
vec = c(rep("Fitted", 83), rep("To predict", 3))
plot_pred_m2$Class = vec

# ----------------------------------------------------------------------------------------------

simu_lines_m2 <- cbind(as.data.frame(summary(fit2, pars = "fake_I", probs = c(0.05, 0.5, 0.95))$summary))
simu_lines_m2 = simu_lines_m2[, -c(ncol(simu_lines_m2)-1, ncol(simu_lines_m2))]
colnames(simu_lines_m2) <- make.names(colnames(simu_lines_m2)) # to remove % in the col names

ind_I1_slm2 = seq(3, nrow(simu_lines_m2), 6)
ind_I2_slm2 = seq(4, nrow(simu_lines_m2), 6)

simu_lines_I_m2 = cbind(simu_lines_m2[ind_I1_slm2, ]*N + simu_lines_m2[ind_I2_slm2, ]*N, 
                     Type = rep("I_hat", length(ind_I2_slm2)),
                     time = seq(1, length(ind_I1_slm2), 1))

simu_lines_I_m2$y = c(datos_nac$casos_total[6:91], rep(NA, nrow(simu_lines_I_m2) - 86))

plot_simu_m2 = simu_lines_I_m2
vec = c(rep("Fitted", 83), rep("To predict", nrow(plot_simu_m2)-83))




# RMSE --------------------------------------------------
topred_m2 = plot_pred_m2[(nrow(plot_pred_m2) - 2):nrow(plot_pred_m2),]
diff_m2 = sum((topred_m2$mean - topred_m2$y)^2)
error1_2 = sqrt(diff_m2/3)

topred_m2 = plot_pred_m2[1:83,]
diff_m2 = sum((topred_m2$mean - topred_m2$y)^2)
error2_2 = sqrt(diff_m2/N)

max_2 = max(simu_lines_I_m2$mean)
date_2 = as.Date("2020-02-26") + 92


