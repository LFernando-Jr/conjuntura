
# Setup -------------------------------------------------------------------

rm(list = ls())

load("dados/pnad.RData")

# Coleta e tratamento de dados --------------------------------------------

data = data_raw %>% 
  pivot_wider(id_cols = date) %>% 
  mutate(date = date,
         inativos = `População` - Total,
         desemprego = `Força de trabalho - desocupada`/`Força de trabalho`*100,
         .keep = "none")

data_ts = ts(log(data$desemprego), start = c(2012, 3), frequency = 12)

data_tsibble = data_ts %>% 
  as_tsibble()

dummies_sazonais = data_ts %>% 
  as_tsibble() %>% 
  mutate(value = value - dplyr::lag(value,1)) %>% 
  mutate(month = factor(
    month(index),
    levels = 1:12,
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
  )) %>% 
  bind_cols(model.matrix(~ month - 1, data = .)) %>% 
  drop_na() %>% 
  as.ts()

plot(data_ts)

# Outliers ----------------------------------------------------------------

diff(data_ts) %>% 
  forecast::ggtsdisplay()

model_isat = diff(data_ts) %>%
  isat(
    iis = TRUE, sis = TRUE,  tis = TRUE,
    plot = TRUE,
    ar = c(1:2),
    uis = dummies_sazonais[,-c(1:2)]
  )

model_isat

outliers = data_tsibble %>% 
  mutate(date = index, 
         d2008 = if_else(as.Date(date) %in% c(as.Date("2008-07-01")),1,0),
         d2020 = if_else(as.Date(date) %in% c(as.Date("2020-04-01")),1,0),
         d2021 = if_else(as.Date(date) %in% c(as.Date("2020-07-01")),1,0),
         .keep  = "none") %>% 
  as_tsibble(index = date) %>% 
  as.ts()

# Modelo de Espaço de Estados ---------------------------------------------

model_ss = SSModel(
  data_ts ~ 
    SSMtrend(degree = 2, Q = list(matrix(NA), matrix(NA))) +
    SSMcycle(period = 12*4.416667, Q = matrix(NA)) +
    SSMseasonal(period = 12, sea.type = "trigonometric", Q = matrix(NA)),
  H = matrix(NA))

custom_fitSSM = function(model, inits) {
  fit = optim(par = inits, fn = function(pars) {
    model$Q[1, 1, 1] <- exp(pars[1])  # Outiliers
    model$Q[2, 2, 1] <- exp(pars[2])  # Outliers
    model$Q[3, 3, 1] <- exp(pars[3])  # Outliers
    model$Q[4, 4, 1] <- exp(pars[4])  # Tendência - nível
    model$Q[5, 5, 1] <- exp(pars[5])  # Tendência - inclinação
    model$Q[6, 6, 1] <- exp(pars[6])  # Sazonalidade
    model$Q[7, 7, 1] <- exp(pars[7])  # Sazonalidade
    model$Q[8, 8, 1] <- exp(pars[8])  # Sazonalidade
    model$Q[9, 9, 1] <- exp(pars[9])  # Sazonalidade
    model$Q[10, 10, 1] <- exp(pars[10])  # Sazonalidade
    model$Q[11, 11, 1] <- exp(pars[11])  # Sazonalidade
    model$Q[12, 12, 1] <- exp(pars[12])  # Sazonalidade
    model$Q[13, 13, 1] <- exp(pars[13])  # Sazonalidade
    model$Q[14, 14, 1] <- exp(pars[14])  # Sazonalidade
    model$Q[15, 15, 1] <- exp(pars[15])  # Sazonalidade
    model$H[1] <- exp(pars[16])  # Erro de observação
    
    # Retorna o negativo da log-verossimilhança para maximização
    - logLik(model)
  }, method = "Nelder-Mead", control = list(maxit = 500))
  
  # Atualiza o modelo com os valores otimizados
  model$Q[1, 1, 1] <- exp(fit$par[1])
  model$Q[2, 2, 1] <- exp(fit$par[2])
  model$Q[3, 3, 1] <- exp(fit$par[3])
  model$Q[4, 4, 1] <- exp(fit$par[4])
  model$Q[5, 5, 1] <- exp(fit$par[5])
  model$Q[6, 6, 1] <- exp(fit$par[6])
  model$Q[7, 7, 1] <- exp(fit$par[7])
  model$Q[8, 8, 1] <- exp(fit$par[8])
  model$Q[9, 9, 1] <- exp(fit$par[9])
  model$Q[10, 10, 1] <- exp(fit$par[10])
  model$Q[11, 11, 1] <- exp(fit$par[11])
  model$Q[12, 12, 1] <- exp(fit$par[12])
  model$Q[13, 13, 1] <- exp(fit$par[13])
  model$Q[14, 14, 1] <- exp(fit$par[14])
  model$Q[15, 15, 1] <- exp(fit$par[15])
  model$H[1] <- exp(fit$par[16])
  
  return(list(model = model, optim.out = fit))
}

# Estimar o modelo usando a função de Perda
fit = custom_fitSSM(model_ss, 
                    inits = c(log(0.999),  # Tendência - nível
                              log(0.999),  # Tendência - inclinação 
                              log(0.01),   # Ciclo econômico
                              log(0.01),   # Sazonalidade
                              log(0.01),   # Sazonalidade
                              log(0.01),   # Sazonalidade
                              log(0.01),   # Sazonalidade
                              log(0.01),   # Sazonalidade
                              log(0.01),   # Sazonalidade
                              log(0.01),   # Sazonalidade
                              log(0.01),   # Sazonalidade
                              log(0.01),   # Sazonalidade 
                              log(0.01),    # Outiliers
                              log(0.01),   # Outiliers
                              log(0.01),   # Outiliers
                              log(0.01)))

# fit = custom_fitSSM(model_ss, inits = rep(log(var(data_ts)), 9))

# fit = fitSSM(model_ss, inits = rep(log(0.01), 10), method = "BFGS")

exp(fit$optim.out$par)

# Filtro de Kalman --------------------------------------------------------

model_kfs = KFS(fit$model, 
                filtering = "state", 
                smoothing = c("state", "mean"))

data = tibble(
  date = time(data_ts) %>% as.Date(),
  trend = model_kfs$alphahat[, "level"],
  cycle = model_kfs$alphahat[, "cycle"],
  residuals = residuals(model_kfs, type = "response"),
  seasonality = data_ts - 
    trend - 
    cycle -
    residuals
  ) %>% 
  mutate(unrate = data_ts,
         unrate_sa = data_ts - seasonality)

# Resultados --------------------------------------------------------------

# Tendência
p1 = data %>% 
  ggplot() +
  aes(x = date, y = trend %>% exp()) + 
  geom_line(color = "blue", linewidth = .75) +
  labs(title = "Componente Tendencial", 
       y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

p1

# Ciclo
p2 = data %>% 
  ggplot() +
  aes(x = date, y = cycle %>% exp()) +
  geom_line(color = "green", linewidth = .75) +
  labs(title = "Componente Cíclico", 
       y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

p2

# Sazonalidade
p3 = data %>% 
  ggplot() +
  aes(x = date, y = seasonality %>% exp()) +
  geom_line(color = "red", linewidth = .75) +
  labs(title = "Componente Sazonal", 
       y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

p3

# Resíduos
p4 = data %>% 
  ggplot() +
  aes(x = date, y = residuals %>% exp()) +
  geom_line(color = "black", linewidth = .75) +
  labs(title = "Componente Residual", 
       y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

p4

# Série
p6 = data %>% 
  ggplot() +
  geom_line(aes(x = date, y = unrate %>% exp()), color = "black", size = 1) +
  geom_line(aes(x = date, y = unrate_sa %>% exp()), color = "red", size = 1) +
                              # Original = as.numeric(data_ts)), 
                              # aes(x = Time, y = Original), color = "black", size = 1) +
  # geom_line(data = data.frame(Time = time(data_ts), 
                              # Original = as.numeric(data_ts)), 
            # aes(x = Time, y = Original), color = "black", size = 1) +
  # geom_line(data = data.frame(Time = time(data_ts),
                              # Combined = data$trend +
                                # data$cycle ),
            # aes(x = Time, y = Combined), color = "red", size = 1) +
  labs(title = "PIB Original vs. Recomposto (Trend + Cycle + Outliers)", 
       y = "Log do PIB", x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

p6

# Combinado ---------------------------------------------------------------

gridExtra::grid.arrange(p1, p2, p3, p4, p6, ncol = 2, nrow = 3)

# Robustez ----------------------------------------------------------------

residuals = data$residuals

residuals %>% summary()

forecast::checkresiduals(residuals,
                         lag = 10)

residuals %>% 
  forecast::ggtsdisplay()

residuals %>%
  isat(
    iis = TRUE, sis = TRUE,  tis = TRUE,
    plot = TRUE
    # uis = dummies_sazonais[,-c(1:2)]
  )

# normalidade
residuals %>% 
  tseries::jarque.bera.test()

residuals %>% 
  shapiro.test()

residuals %>% 
  density() %>% plot()

# heterocedasticidade
residuals %>% 
  feasts::stat_arch_lm()

