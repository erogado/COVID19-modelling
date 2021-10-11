#load("results_SEI1I2RD_def.Rdata")
#load("extended_model.Rdata")
theme_set(theme_bw())

# Estudio de convergencia -------------------------------------------
library(deSolve)
library(dplyr)
library(rstan)
library(outbreaks)
library(bayesplot)

stan_dens(fit, pars = c("theta"), ncol = 4, separate_chains = TRUE) +
  theme(legend.position = "bottom")

# Getting the parameter results ------------------------------------
pars = c("theta", "R_0")
print(fit, pars = pars)

post = extract(fit)
post_theta = data.frame(post$theta)
colnames(post_theta) = c("beta1", "beta2", "epsilon", "kappa", "lambda", "gamma1", "gamma2", "mu","omega", "eta")

library(coda)

HDP_inter = NULL
for(i in 1:ncol(post_theta)) {
  data = post_theta[, i]
  inter = HPDinterval(as.mcmc(data), prob = 0.95)
  m = mean(data)
  a = inter[1]
  b = inter[2]
  amb = c(a, m, b)
  HDP_inter = rbind(HDP_inter, amb)
  
}
HDP_inter = data.frame(HDP_inter)

colnames(HDP_inter) = c("25", "mean", "975")
rownames(HDP_inter) = c("beta1", "beta2", "epsilon",  "kappa", "lambda", "gamma1", "gamma2", "mu", "omega", "eta")

HDP_inter

post_R0 = data.frame(post$R_0)
mean(post_R0$post.R_0)
HPDinterval(as.mcmc(post_R0$post.R_0), prob = 0.95)

stan_dens(fit, pars = c("R_0"), ncol = 1, separate_chains = TRUE) +  xlab("") +
  theme(legend.position = "bottom")

# Plotting the results ------------------------------------------------
fitted_lines = cbind(as.data.frame(summary(fit, pars = "y_hat", probs = c(0.05, 0.5, 0.95))$summary))
fitted_lines = fitted_lines[, -c(ncol(fitted_lines)-1, ncol(fitted_lines))]
colnames(fitted_lines) <- make.names(colnames(fitted_lines)) # to remove % in the col names

ind_I1 = seq(3, nrow(fitted_lines), 6)
ind_I2 = seq(4, nrow(fitted_lines), 6)

y1 = datos_nac$casos_total[6:88] - datos_nac$hospitalizados[6:88]
y2 = datos_nac$hospitalizados[6:88]
y = datos_nac$casos_total[6:88]

fitted_lines_I1 = cbind(fitted_lines[ind_I1, ]*N, 
                        Type = rep("I1_hat", length(ind_I1)),
                        time = as.Date(datos_nac$Fecha[6:88]), I1 = y1)

fitted_lines_I2 = cbind(fitted_lines[ind_I2, ]*N, 
                        Type = rep("I2_hat", length(ind_I2)), 
                        time = as.Date(datos_nac$Fecha[6:88]), I2 = y2)

fitted_lines_I = cbind(fitted_lines[ind_I1, ]*N + fitted_lines[ind_I2, ]*N, 
                     Type = rep("I_hat", length(ind_I2)),
                     time = seq(1, length(datos_nac$Fecha[6:88]), 1), I = y)



plt_adj = ggplot(fitted_lines_I, mapping = aes(x = time)) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = "darkslategray4", alpha = 0.4) +
  geom_line(mapping = aes(x = time, y = X50.), col = "red", size = 1) + 
  geom_point(mapping = aes(y = y)) +
  ggtitle("") + xlab("") + ylab("") 

ggplot(fitted_lines_I1, mapping = aes(x = time)) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = "darkslategray4", alpha = 0.4) +
  geom_line(mapping = aes(x = time, y = X50.), col = "red", size = 1) + 
  geom_point(mapping = aes(y = y1)) +
  ggtitle("Total infected fit") + xlab("") + ylab("")  + theme_bw()

ggplot(fitted_lines_I2, mapping = aes(x = time)) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = "darkslategray4", alpha = 0.4) +
  geom_line(mapping = aes(x = time, y = X50.), col = "red", size = 1) + 
  geom_point(mapping = aes(y = I2)) +
  ggtitle("Total infected fit") + xlab("") + ylab("")  + theme_bw()

# Plot with the predictions -------------------------------------------
pred_lines <- cbind(as.data.frame(summary(fit, pars = "fake_I", probs = c(0.05, 0.5, 0.95))$summary))
pred_lines = pred_lines[, -c(ncol(pred_lines)-1, ncol(pred_lines))]
colnames(pred_lines) <- make.names(colnames(pred_lines)) # to remove % in the col names

ind_I1 = seq(3, nrow(pred_lines), 6)
ind_I2 = seq(4, nrow(pred_lines), 6)

pred_lines_I = cbind(pred_lines[ind_I1, ]*N + pred_lines[ind_I2, ]*N, 
                       Type = rep("I_hat", length(ind_I2)),
                       time = seq(1, length(ind_I1), 1))

pred_lines_I$y = c(datos_nac$casos_total[6:91], rep(NA, nrow(pred_lines_I) - 86))

plot_pred = pred_lines_I[1:86, ]
vec = c(rep("Fitted", 83), rep("To predict", 3))
plot_pred$Class = vec

plt_fit = ggplot(plot_pred, mapping = aes(x = time)) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = "darkslategray4", alpha = 0.4) +
  geom_line(mapping = aes(x = time, y = X50.), col = "red", size = 1) + 
  geom_point(mapping = aes(y = y, colour = Class)) +
  ggtitle("") + xlab("") + ylab("")  +  
  scale_colour_manual("", values = c("black", "cyan3")) +
  theme(legend.position = "none")

# Plot the curve -------------------------------------------
simu_lines <- cbind(as.data.frame(summary(fit, pars = "fake_I", probs = c(0.05, 0.5, 0.95))$summary))
simu_lines = simu_lines[, -c(ncol(simu_lines)-1, ncol(simu_lines))]
colnames(simu_lines) <- make.names(colnames(simu_lines)) # to remove % in the col names

ind_I1 = seq(3, nrow(simu_lines), 6)
ind_I2 = seq(4, nrow(simu_lines), 6)

simu_lines_I = cbind(simu_lines[ind_I1, ]*N + simu_lines[ind_I2, ]*N, 
                     Type = rep("I_hat", length(ind_I2)),
                     time = seq(1, length(ind_I1), 1))

simu_lines_I$y = c(datos_nac$casos_total[6:91], rep(NA, nrow(simu_lines_I) - 86))

plot_simu = simu_lines_I
vec = c(rep("Fitted", 83), rep("To predict", nrow(plot_simu)-83))
plot_pred$Class = vec

plt_sim = ggplot(plot_simu, mapping = aes(x = time)) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = "darkslategray4", alpha = 0.4) +
  geom_line(mapping = aes(x = time, y = X50.), col = "red", size = 1) + 
  geom_point(mapping = aes(y = y)) +
  ggtitle("") + xlab("") + ylab("")  + theme_bw()



# Merging the plots and calculating the error ---------------------------------------
library(gridExtra)
grid.arrange(plt_adj, plt_fit, plt_sim) 

topred = plot_pred[(nrow(plot_pred) - 2):nrow(plot_pred),]
diff = sum((topred$mean - topred$y)^2)
sqrt(diff/3)

topred = plot_pred[1:83,]
diff = sum((topred$mean - topred$y)^2)
sqrt(diff/N)


# Plotting the infected evolution separately -------------------------------------
model_evol = cbind(as.data.frame(summary(fit, pars = "fake_I", probs = c(0.05, 0.5, 0.95))$summary))
model_evol = model_evol[, -c(ncol(model_evol)-1, ncol(model_evol))]
colnames(model_evol) <- make.names(colnames(model_evol)) # to remove % in the col names

ind_S = seq(1, nrow(model_evol), 6)
ind_E = seq(2, nrow(model_evol), 6)
ind_I1 = seq(3, nrow(model_evol), 6)
ind_I2 = seq(4, nrow(model_evol), 6)
ind_R = seq(5, nrow(model_evol), 6)
ind_D = seq(6, nrow(model_evol), 6)

y1 = datos_nac$casos_total[6:88] - datos_nac$hospitalizados[6:88]
y2 = datos_nac$hospitalizados[6:88]
y = datos_nac$casos_total[6:88]


mevol_S = cbind(model_evol[ind_S, ]*N)[, c(1, 4, 5)]
mevol_S$Type = rep("S_hat", length(ind_I2))

mevol_E = cbind(model_evol[ind_E, ]*N)[, c(1, 4, 5)]
mevol_E$Type = rep("E_hat", length(ind_I2))

mevol_I1 = cbind(model_evol[ind_I1, ]*N)[, c(1, 4, 5)]
mevol_I1$Type = rep("I1_hat", length(ind_I2))

mevol_I2 = cbind(fitted_lines[ind_I2, ]*N)[, c(1, 4, 5)]
mevol_I2$Type = rep("I2_hat", length(ind_I2))

mevol_R = cbind(fitted_lines[ind_R, ]*N)[, c(1, 4, 5)]
mevol_R$Type = rep("R_hat", length(ind_I2))

mevol_D = cbind(fitted_lines[ind_D, ]*N)[, c(1, 4, 5)]
mevol_D$Type = rep("D_hat", length(ind_I2))

mevol_plot = rbind(mevol_S, mevol_E, mevol_I1, mevol_I2, mevol_R, mevol_D)
mevol_plot$Time = c(1:length(ind_I2), 1:length(ind_I2), 1:length(ind_I2), 1:length(ind_I2), 1:length(ind_I2), 1:length(ind_I2))

ggplot(mevol_plot) +
  geom_line(aes(x = Time, y = mean, colour = Type))


# Plotting ----------------------------------
fitted_lines = cbind(as.data.frame(summary(fit, pars = "fake_I", probs = c(0.05, 0.5, 0.95))$summary))
fitted_lines = fitted_lines[, -c(ncol(fitted_lines)-1, ncol(fitted_lines))]
colnames(fitted_lines) <- make.names(colnames(fitted_lines)) # to remove % in the col names

ind_I1 = seq(3, nrow(fitted_lines), 6)
ind_I2 = seq(4, nrow(fitted_lines), 6)

y1 = datos_nac$casos_total[6:88] - datos_nac$hospitalizados[6:88]
y2 = datos_nac$hospitalizados[6:88]

pred_lines_I1 = cbind(fitted_lines[ind_I1, ]*N, 
                      Type = rep("I1_hat", length(ind_I1)),
                      time = seq(1, length(ind_I1)), 
                      I1 = c(y1, rep(NA, (length(ind_I1) - length(y1)))))


pred_lines_I2 = cbind(fitted_lines[ind_I2, ]*N, 
                      Type = rep("I2_hat", length(ind_I2)),
                      time = seq(1, length(ind_I2)), 
                      I2 = c(y2, rep(NA, (length(ind_I2) - length(y2)))))

ggplot(pred_lines_I1, mapping = aes(x = time)) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = "darkslategray4", alpha = 0.4) +
  geom_line(mapping = aes(x = time, y = X50.), col = "red", size = 1) + 
  geom_point(mapping = aes(y = I1)) +
  ggtitle("Total infected fit") + xlab("") + ylab("")  + theme_bw()

ggplot(pred_lines_I2, mapping = aes(x = time)) +
  geom_ribbon(aes(ymin = X5., ymax = X95.), fill = "darkslategray4", alpha = 0.4) +
  geom_line(mapping = aes(x = time, y = X50.), col = "red", size = 1) + 
  geom_point(mapping = aes(y = I2)) +
  ggtitle("Total infected fit") + xlab("") + ylab("")  + theme_bw()

data = cbind.data.frame(I1 = pred_lines_I1$mean, I1_5 = pred_lines_I1$X5., I1_95 = pred_lines_I1$X95.,
      I2 = pred_lines_I2$mean, I2_5 = pred_lines_I2$X5., I2_95 = pred_lines_I2$X95., time = pred_lines_I1$time,
      I1_real = c(y1, rep(NA, (length(ind_I1) - length(y1)))),
      I2_real = c(y2, rep(NA, (length(ind_I2) - length(y2)))))

ggplot(data, mapping = aes(x = time)) +
  geom_ribbon(aes(ymin = I1_5, ymax = I1_95), fill = "darkslategray4", alpha = 0.4) +
  geom_line(mapping = aes(x = time, y = I1), col = "red", size = 1) + 
  geom_ribbon(aes(ymin = I2_5, ymax = I2_95), fill = "darkslategray4", alpha = 0.4) +
  geom_line(mapping = aes(x = time, y = I2), col = "cyan4", size = 1) +
  geom_point(mapping = aes(y = I1_real), colour = "pink") +
  geom_point(mapping = aes(y = I2_real), colour = "darkblue") +
  ggtitle("Total infected fit") + xlab("") + ylab("")  + theme()
