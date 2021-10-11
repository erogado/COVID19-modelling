load("results_SEI1I2RD_def.Rdata")

fit1 = fit
rm(fit)

# posterior values ---------------------------------------------------------------------------------------------
post_m1 = extract(fit1)
post_theta_m1 = data.frame(post_m1$theta, post_m1$R_0)
colnames(post_theta_m1) = c("beta1", "beta2", "epsilon", "kappa", "lambda", "gamma1", "gamma2", "mu","omega", "R0")

HDP_inter_m1 = NULL
for(i in 1:ncol(post_theta_m1)) {
  data = post_theta_m1[, i]
  inter = HPDinterval(as.mcmc(data), prob = 0.95)
  m = mean(data)
  a = inter[1]
  b = inter[2]
  amb = c(a, m, b)
  HDP_inter_m1 = rbind(HDP_inter_m1, amb)
  
}
HDP_inter_m1 = data.frame(HDP_inter_m1)
colnames(HDP_inter_m1) = c("2.5%", "50%", "97.5%")
rownames(HDP_inter_m1) = c("beta1", "beta2", "epsilon",  "kappa", "lambda", "gamma1", "gamma2", "mu", "omega", "R0")

rm(a, amb, b, data, i, m, inter)

# plots for the results -------------------------------------------------------------------
fitted_lines_m1 = cbind(as.data.frame(summary(fit1, pars = "y_hat", probs = c(0.05, 0.5, 0.95))$summary))
fitted_lines_m1 = fitted_lines_m1[, -c(ncol(fitted_lines_m1)-1, ncol(fitted_lines_m1))]
colnames(fitted_lines_m1) <- make.names(colnames(fitted_lines_m1)) # to remove % in the col names

ind_I1_flm1 = seq(3, nrow(fitted_lines_m1), 6)
ind_I2_flm1 = seq(4, nrow(fitted_lines_m1), 6)

y1 = datos_nac$casos_total[6:88] - datos_nac$hospitalizados[6:88]
y2 = datos_nac$hospitalizados[6:88]
y = datos_nac$casos_total[6:88]

fitted_lines_I_m1 = cbind(fitted_lines_m1[ind_I1_flm1, ]*N + fitted_lines_m1[ind_I2_flm1, ]*N, 
                          Type = rep("I_hat", length(ind_I2_flm1)),
                          time = seq(1, length(datos_nac$Fecha[6:88]), 1), I = y)

# ------------------------------------------------------------------------------------------------
pred_lines_m1 <- cbind(as.data.frame(summary(fit1, pars = "fake_I", probs = c(0.05, 0.5, 0.95))$summary))
pred_lines_m1 = pred_lines_m1[, -c(ncol(pred_lines_m1)-1, ncol(pred_lines_m1))]
colnames(pred_lines_m1) <- make.names(colnames(pred_lines_m1)) # to remove % in the col names

ind_I1_plm1 = seq(3, nrow(pred_lines_m1), 6)
ind_I2_plm1 = seq(4, nrow(pred_lines_m1), 6)

pred_lines_I_m1 = cbind(pred_lines_m1[ind_I1_plm1, ]*N + pred_lines_m1[ind_I2_plm1, ]*N, 
                        Type = rep("I_hat", length(ind_I2_plm1)),
                        time = seq(1, length(ind_I1_plm1), 1))

pred_lines_I_m1$y = c(datos_nac$casos_total[6:91], rep(NA, nrow(pred_lines_I_m1) - 86))

plot_pred_m1 = pred_lines_I_m1[1:86, ]
vec = c(rep("Fitted", 83), rep("To predict", 3))
plot_pred_m1$Class = vec

# ----------------------------------------------------------------------------------------------

simu_lines_m1 <- cbind(as.data.frame(summary(fit1, pars = "fake_I", probs = c(0.05, 0.5, 0.95))$summary))
simu_lines_m1 = simu_lines_m1[, -c(ncol(simu_lines_m1)-1, ncol(simu_lines_m1))]
colnames(simu_lines_m1) <- make.names(colnames(simu_lines_m1)) # to remove % in the col names

ind_I1_slm1 = seq(3, nrow(simu_lines_m1), 6)
ind_I2_slm1 = seq(4, nrow(simu_lines_m1), 6)

simu_lines_I_m1 = cbind(simu_lines_m1[ind_I1_slm1, ]*N + simu_lines_m1[ind_I2_slm1, ]*N, 
                        Type = rep("I_hat", length(ind_I2_slm1)),
                        time = seq(1, length(ind_I1_slm1), 1))

simu_lines_I_m1$y = c(datos_nac$casos_total[6:91], rep(NA, nrow(simu_lines_I_m1) - 86))

plot_simu_m1 = simu_lines_I_m1
vec = c(rep("Fitted", 83), rep("To predict", nrow(plot_simu_m1)-83))




# RMSE --------------------------------------------------
topred_m1 = plot_pred_m1[(nrow(plot_pred_m1) - 2):nrow(plot_pred_m1),]
diff_m1 = sum((topred_m1$mean - topred_m1$y)^2)
error1_1 = sqrt(diff_m1/3)

topred_m1 = plot_pred_m1[1:83,]
diff_m1 = sum((topred_m1$mean - topred_m1$y)^2)
error2_1 = sqrt(diff_m1/N)

max_1 = max(simu_lines_I_m1$mean)
date_1 = as.Date("2020-02-26") + 151

simu_lines_I_m1[ simu_lines_I_m1$mean == max_1, ]
